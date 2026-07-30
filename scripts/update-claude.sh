#!/usr/bin/env bash
# ABOUTME: Updates the claude package to a specific or latest version
# ABOUTME: Fetches Anthropic's official release manifest (version + per-platform checksums)

set -euo pipefail

PKG_DIR="pkgs/claude"
MANIFEST_FILE="$PKG_DIR/manifest.json"
BASE_URL="https://downloads.claude.ai/claude-code-releases"

# Get version from argument or fetch latest
if [[ -n "${1:-}" ]]; then
    VERSION="$1"
    echo "Target version: $VERSION"
else
    echo "Fetching latest version..."
    VERSION=$(curl -fsSL "$BASE_URL/latest")
    echo "Latest version: $VERSION"
fi

# Current version
CURRENT=$(jq -r '.version' "$MANIFEST_FILE")
echo "Current version: $CURRENT"

if [[ "$CURRENT" == "$VERSION" ]]; then
    echo "Already at version $VERSION"
    exit 0
fi

echo ""
echo "Updating $CURRENT -> $VERSION"
echo ""

echo "Fetching manifest for v${VERSION}..."
MANIFEST=$(curl -fsSL "$BASE_URL/$VERSION/manifest.json")

MANIFEST_VERSION=$(echo "$MANIFEST" | jq -r '.version')
if [[ "$MANIFEST_VERSION" != "$VERSION" ]]; then
    echo "Error: manifest version ($MANIFEST_VERSION) does not match requested version ($VERSION)"
    exit 1
fi

echo "$MANIFEST" | jq '.' > "$MANIFEST_FILE"

# Verify build
echo ""
echo "Verifying build..."
if nix build .#claude 2>&1; then
    echo ""
    echo "Successfully updated claude: $CURRENT -> $VERSION"
    rm -f result
else
    echo ""
    echo "Error: Build failed"
    exit 1
fi
