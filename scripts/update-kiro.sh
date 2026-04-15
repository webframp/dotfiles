#!/usr/bin/env bash
# ABOUTME: Updates kiro-cli package to a specific or latest version
# ABOUTME: Fetches version and hashes from the kiro CLI manifest

set -euo pipefail

PKG_FILE="pkgs/kiro-cli/default.nix"
MANIFEST_URL="https://prod.download.cli.kiro.dev/stable/latest/manifest.json"

# Get version from argument or fetch latest from manifest
if [[ -n "${1:-}" ]]; then
    VERSION="$1"
    echo "Target version: $VERSION"
else
    echo "Fetching latest version from manifest..."
    VERSION=$(curl -s "$MANIFEST_URL" | jq -r '.version')
    echo "Latest version: $VERSION"
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

# Fetch manifest for the target version
VERSIONED_MANIFEST_URL="https://prod.download.cli.kiro.dev/stable/${VERSION}/manifest.json"
echo "Fetching manifest for v${VERSION}..."
MANIFEST=$(curl -s "$VERSIONED_MANIFEST_URL")

if [[ "$(echo "$MANIFEST" | jq -r '.version // empty')" != "$VERSION" ]]; then
    # Fall back to latest manifest if versioned one doesn't work
    MANIFEST=$(curl -s "$MANIFEST_URL")
    MANIFEST_VERSION=$(echo "$MANIFEST" | jq -r '.version')
    if [[ "$MANIFEST_VERSION" != "$VERSION" ]]; then
        echo "Error: Could not find manifest for version $VERSION (latest is $MANIFEST_VERSION)"
        exit 1
    fi
fi

# Platform configs: nix system -> manifest query
# x86_64-linux: headless tar.gz
# aarch64-darwin: universal DMG
SYSTEMS=("x86_64-linux" "aarch64-darwin")
HASHES=()

for system in "${SYSTEMS[@]}"; do
    if [[ "$system" == "x86_64-linux" ]]; then
        hex_hash=$(echo "$MANIFEST" | jq -r '.packages[] | select(.architecture == "x86_64" and .os == "linux" and .fileType == "tarGz" and .variant == "headless" and (.targetTriple | test("gnu"))) | .sha256')
    elif [[ "$system" == "aarch64-darwin" ]]; then
        hex_hash=$(echo "$MANIFEST" | jq -r '.packages[] | select(.os == "macos" and .fileType == "dmg") | .sha256')
    fi

    if [[ -z "$hex_hash" ]]; then
        echo "Error: Could not find hash for $system in manifest"
        exit 1
    fi

    sri_hash=$(nix hash convert --hash-algo sha256 --to sri "$hex_hash")
    HASHES+=("$sri_hash")
    echo "  $system: $sri_hash"
done

echo ""
echo "Updating $PKG_FILE..."

# Update version (use temp file for BSD/GNU sed compatibility)
sed "s/version = \"$CURRENT\"/version = \"$VERSION\"/" "$PKG_FILE" > "$PKG_FILE.tmp" && mv "$PKG_FILE.tmp" "$PKG_FILE"

# Update hashes for each platform
for i in "${!SYSTEMS[@]}"; do
    system="${SYSTEMS[$i]}"
    hash="${HASHES[$i]}"
    # Match the hash line within the system's block
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
if nix build .#kiro-cli 2>&1; then
    echo ""
    echo "Successfully updated kiro-cli: $CURRENT -> $VERSION"
    rm -f result
else
    echo ""
    echo "Error: Build failed"
    exit 1
fi
