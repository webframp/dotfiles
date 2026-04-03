#!/usr/bin/env bash
# ABOUTME: Updates swamp package to a specific or latest version
# ABOUTME: Fetches prebuilt binary hashes for all supported platforms

set -euo pipefail

PKG_FILE="pkgs/swamp/default.nix"
REPO="systeminit/swamp"

# Get version from argument or fetch latest
if [[ -n "${1:-}" ]]; then
    VERSION="$1"
    echo "Target version: $VERSION"
else
    echo "Fetching latest release..."
    # Strip leading 'v' from tag
    VERSION=$(curl -s "https://api.github.com/repos/$REPO/releases/latest" | jq -r '.tag_name' | sed 's/^v//')
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

# Platform configs: system and binary suffix (parallel arrays for bash 3.2 compatibility)
SYSTEMS=("x86_64-linux" "aarch64-darwin")
SUFFIXES=("linux-x86_64" "darwin-aarch64")
HASHES=()

for i in "${!SYSTEMS[@]}"; do
    system="${SYSTEMS[$i]}"
    suffix="${SUFFIXES[$i]}"
    url="https://github.com/$REPO/releases/download/v${VERSION}/swamp-${suffix}"

    echo "Fetching hash for $system..."

    # Verify URL exists
    if ! curl -sfI "$url" > /dev/null 2>&1; then
        echo "Error: Release asset not found: $url"
        exit 1
    fi

    # Get hash (no --unpack since these are plain binaries)
    base32_hash=$(nix-prefetch-url "$url" 2>/dev/null)
    sri_hash=$(nix hash convert --hash-algo sha256 --to sri "$base32_hash")
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
if nix build .#swamp 2>&1; then
    echo ""
    echo "Successfully updated swamp: $CURRENT -> $VERSION"
    rm -f result
else
    echo ""
    echo "Error: Build failed"
    exit 1
fi
