#!/usr/bin/env bash
# ABOUTME: Updates coder package to a specific or latest version
# ABOUTME: Fetches prebuilt binary hashes for all supported platforms

set -euo pipefail

PKG_FILE="pkgs/coder/default.nix"
REPO="coder/coder"

# Get version from argument or fetch from server
if [[ -n "${1:-}" ]]; then
    VERSION="$1"
    echo "Target version: $VERSION"
else
    CODER_URL=$(cat ~/.config/coderv2/url 2>/dev/null || true)
    if [[ -z "$CODER_URL" ]]; then
        echo "Error: No version specified and no Coder server URL found in ~/.config/coderv2/url"
        exit 1
    fi
    echo "Fetching server version from $CODER_URL..."
    VERSION=$(curl -s "$CODER_URL/api/v2/buildinfo" | jq -r '.version' | sed 's/^v//; s/+.*//')
    echo "Server version: $VERSION"
fi

# Current version
CURRENT=$(grep 'version = ' "$PKG_FILE" | head -1 | sed 's/.*"\([^"]*\)".*/\1/')
echo "Current version: $CURRENT"

if [[ "$CURRENT" == "$VERSION" ]]; then
    echo "Already at version $VERSION"
    exit 0
fi

echo ""
echo "Updating $CURRENT -> $VERSION"
echo ""

# Platform configs: system -> (url_suffix, extension)
declare -A PLATFORMS=(
    ["x86_64-linux"]="linux_amd64.tar.gz"
    ["aarch64-darwin"]="darwin_arm64.zip"
)

declare -A HASHES

for system in "${!PLATFORMS[@]}"; do
    suffix="${PLATFORMS[$system]}"
    url="https://github.com/$REPO/releases/download/v${VERSION}/coder_${VERSION}_${suffix}"

    echo "Fetching hash for $system..."

    # Verify URL exists
    if ! curl -sfI "$url" > /dev/null 2>&1; then
        echo "Error: Release asset not found: $url"
        exit 1
    fi

    # Get hash (--unpack to match fetchzip behavior)
    base32_hash=$(nix-prefetch-url --unpack "$url" 2>/dev/null)
    sri_hash=$(nix hash convert --hash-algo sha256 --to sri "$base32_hash")
    HASHES[$system]="$sri_hash"
    echo "  $system: $sri_hash"
done

echo ""
echo "Updating $PKG_FILE..."

# Update version
sed -i "s/version = \"$CURRENT\"/version = \"$VERSION\"/" "$PKG_FILE"

# Update hashes for each platform
for system in "${!HASHES[@]}"; do
    hash="${HASHES[$system]}"
    # Match the hash line within the system's block
    # Use awk to find the right section and update the hash
    awk -v sys="$system" -v hash="$hash" '
        /'"$system"'/ { in_block=1 }
        in_block && /hash = "sha256-[^"]*"/ {
            sub(/hash = "sha256-[^"]*"/, "hash = \"" hash "\"")
            in_block=0
        }
        { print }
    ' "$PKG_FILE" > "$PKG_FILE.tmp" && mv "$PKG_FILE.tmp" "$PKG_FILE"
done

# Verify build
echo ""
echo "Verifying build..."
if nix build .#coder 2>&1; then
    echo ""
    echo "Successfully updated coder: $CURRENT -> $VERSION"
    rm -f result
else
    echo ""
    echo "Error: Build failed"
    exit 1
fi
