#!/usr/bin/env bash
# claude-code-status dispatcher. Routes statusLine rendering to whichever
# claude-code-status plugin version is currently installed, so a
# `/plugin update` doesn't leave settings.json pointing at a stale cache path.
set -u

installed="$HOME/.claude/plugins/installed_plugins.json"
cmd=""

if [ -f "$installed" ] && command -v jq >/dev/null 2>&1; then
  path=$(jq -r '.plugins["claude-code-status@webframp"][0].installPath // ""' "$installed" 2>/dev/null)
  [ -n "$path" ] && [ -x "$path/bin/statusline.sh" ] && cmd="$path/bin/statusline.sh"
fi

# Fallback: any installed version under the plugin cache.
if [ -z "$cmd" ]; then
  for s in "$HOME"/.claude/plugins/cache/webframp/claude-code-status/*/bin/statusline.sh; do
    [ -x "$s" ] && cmd="$s" && break
  done
fi

[ -n "$cmd" ] && exec "$cmd"
exit 0
