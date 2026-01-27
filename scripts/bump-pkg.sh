#!/usr/bin/env bash
# ABOUTME: Bumps a custom package to its latest GitHub release
# ABOUTME: Updates version and hashes in pkgs/<name>/default.nix

set -euo pipefail

PKG="${1:-}"
if [[ -z "$PKG" ]]; then
    echo "Usage: $0 <package-name>"
    exit 1
fi

PKG_FILE="pkgs/$PKG/default.nix"
if [[ ! -f "$PKG_FILE" ]]; then
    echo "Error: Package file not found: $PKG_FILE"
    exit 1
fi

# Extract owner and repo from fetchFromGitHub
OWNER=$(grep -A5 'fetchFromGitHub' "$PKG_FILE" | grep 'owner' | sed 's/.*"\([^"]*\)".*/\1/')
REPO=$(grep -A5 'fetchFromGitHub' "$PKG_FILE" | grep 'repo' | sed 's/.*"\([^"]*\)".*/\1/')

if [[ -z "$OWNER" || -z "$REPO" ]]; then
    echo "Error: Could not extract owner/repo from $PKG_FILE"
    exit 1
fi

echo "Package: $PKG"
echo "GitHub: $OWNER/$REPO"

# Get current version
CURRENT_VERSION=$(grep 'version = ' "$PKG_FILE" | head -1 | sed 's/.*"\([^"]*\)".*/\1/')
echo "Current version: $CURRENT_VERSION"

# Get latest release from GitHub API (no gh cli required)
LATEST_TAG=$(curl -s "https://api.github.com/repos/$OWNER/$REPO/releases/latest" | jq -r '.tag_name // empty')
if [[ -z "$LATEST_TAG" ]]; then
    # Fallback to tags if no releases (some repos use tags only)
    LATEST_TAG=$(curl -s "https://api.github.com/repos/$OWNER/$REPO/tags" | jq -r '.[0].name // empty')
fi
if [[ -z "$LATEST_TAG" ]]; then
    echo "Error: Could not fetch latest release/tag from GitHub"
    exit 1
fi

# Detect if upstream uses 'v' prefix
if [[ "$LATEST_TAG" == v* ]]; then
    HAS_V_PREFIX=true
    LATEST_VERSION="${LATEST_TAG#v}"
    echo "Latest version: $LATEST_VERSION (tag: $LATEST_TAG, v-prefixed)"
else
    HAS_V_PREFIX=false
    LATEST_VERSION="$LATEST_TAG"
    echo "Latest version: $LATEST_VERSION (tag: $LATEST_TAG, no prefix)"
fi

if [[ "$CURRENT_VERSION" == "$LATEST_VERSION" ]]; then
    echo "Already at latest version"
    exit 0
fi

echo ""
echo "Updating $CURRENT_VERSION -> $LATEST_VERSION"

# Check current rev pattern in nix file
CURRENT_REV_LINE=$(grep 'rev = ' "$PKG_FILE" | head -1)

# Determine expected rev pattern based on upstream tag format
if [[ "$HAS_V_PREFIX" == true ]]; then
    # Upstream uses v-prefix, rev should be: rev = "v${version}";
    EXPECTED_REV_PATTERN='rev = "v\${version}";'
    if ! echo "$CURRENT_REV_LINE" | grep -q 'v\${version}'; then
        echo "Updating rev pattern to use v-prefix..."
        # Handle various patterns: rev = version; or rev = "${version}"; -> rev = "v${version}";
        sed -i '' 's/rev = version;/rev = "v\${version}";/' "$PKG_FILE"
        sed -i '' 's/rev = "\${version}";/rev = "v\${version}";/' "$PKG_FILE"
    fi
else
    # Upstream has no v-prefix, rev should be: rev = version; or rev = "${version}";
    if echo "$CURRENT_REV_LINE" | grep -q 'v\${version}'; then
        echo "Updating rev pattern to remove v-prefix..."
        sed -i '' 's/rev = "v\${version}";/rev = version;/' "$PKG_FILE"
    fi
fi

# Update version
sed -i '' "s/version = \"$CURRENT_VERSION\"/version = \"$LATEST_VERSION\"/" "$PKG_FILE"

# Clear hashes to trigger recalculation
sed -i '' 's/hash = "sha256-[^"]*"/hash = ""/' "$PKG_FILE"
sed -i '' 's/vendorHash = "sha256-[^"]*"/vendorHash = ""/' "$PKG_FILE"

# Build to get source hash
echo ""
echo "Calculating source hash..."
SRC_HASH=$(nix build ".#$PKG" 2>&1 | grep 'got:' | awk '{print $2}' || echo "")
if [[ -z "$SRC_HASH" ]]; then
    echo "Error: Could not determine source hash"
    exit 1
fi
echo "Source hash: $SRC_HASH"
sed -i '' "s|hash = \"\"|hash = \"$SRC_HASH\"|" "$PKG_FILE"

# Build to get vendor hash (or detect vendored deps)
echo ""
echo "Calculating vendor hash..."
VENDOR_OUTPUT=$(nix build ".#$PKG" 2>&1 || true)

if echo "$VENDOR_OUTPUT" | grep -q "vendor folder exists"; then
    echo "Vendor hash: null (vendored dependencies)"
    sed -i '' 's/vendorHash = ""/vendorHash = null/' "$PKG_FILE"
else
    VENDOR_HASH=$(echo "$VENDOR_OUTPUT" | grep 'got:' | awk '{print $2}' || echo "")
    if [[ -n "$VENDOR_HASH" ]]; then
        echo "Vendor hash: $VENDOR_HASH"
        sed -i '' "s|vendorHash = \"\"|vendorHash = \"$VENDOR_HASH\"|" "$PKG_FILE"
    fi
fi

# Final verification build
echo ""
echo "Verifying build..."
if nix build ".#$PKG" 2>&1; then
    echo ""
    echo "Successfully updated $PKG: $CURRENT_VERSION -> $LATEST_VERSION"
    rm -f result
else
    echo ""
    echo "Error: Final build failed"
    exit 1
fi
