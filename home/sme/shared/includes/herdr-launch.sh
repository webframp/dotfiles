# ABOUTME: herdr popup helpers: fzf project picker and agent-in-a-new-worktree launcher
# ABOUTME: Packaged by modules/home-manager/herdr.nix; run from [[keys.command]] popups
#
#   herdr-launch project   pick a directory (zoxide frecency first, then git
#                          repos under HERDR_PROJECT_ROOTS) and focus or open
#                          a workspace on it
#   herdr-launch agent KIND...
#                          pick an agent kind, create a git worktree off the
#                          active repo, and start the agent in it

herdr=${HERDR_BIN_PATH:-herdr}
project_roots=${HERDR_PROJECT_ROOTS:-$HOME/src}
worktrees_dir=${HERDR_WORKTREES_DIR:-$HOME/.herdr/worktrees}

# Popups close when the command exits, so hold errors on screen.
fail() {
  printf '\n%s\n' "$*" >&2
  read -rsn1 -p 'press any key' </dev/tty || true
  exit 1
}

# Run a herdr API command, print its JSON, and fail on an error response.
call() {
  local out
  out=$("$herdr" "$@" 2>&1) || true
  if ! jq -e 'has("result")' >/dev/null 2>&1 <<<"$out"; then
    fail "herdr $1 $2: $(jq -r '.error.message // empty' 2>/dev/null <<<"$out" || printf '%s' "$out")"
  fi
  printf '%s\n' "$out"
}

expand_tilde() {
  printf '%s\n' "${1/#\~/$HOME}"
}

project_candidates() {
  zoxide query --list 2>/dev/null || true
  local root roots
  IFS=: read -ra roots <<<"$project_roots"
  for root in "${roots[@]}"; do
    root=$(expand_tilde "$root")
    [ -d "$root" ] || continue
    fd --hidden --type d --max-depth 4 --prune --format '{//}' '^\.git$' "$root" 2>/dev/null | sort
  done
}

cmd_project() {
  local wt pick dir ws
  wt=$(expand_tilde "$worktrees_dir")
  # Keep zoxide's order (recency) and drop duplicates and agent worktrees.
  # fzf's path scheme ranks basename matches first; ties keep input order.
  pick=$(project_candidates |
    awk -v wt="$wt/" -v home="$HOME" '
      index($0, wt) == 1 || seen[$0]++ { next }
      { d = $0; if (index(d, home "/") == 1) d = "~" substr(d, length(home) + 1); print $0 "\t" d }' |
    fzf --delimiter='\t' --with-nth=2 --scheme=path --tiebreak=pathname,index \
      --prompt='project> ' --preview='ls -A --color=always {1}' --preview-window=right,40%) || exit 0
  dir=$(realpath "${pick%%$'\t'*}")
  zoxide add "$dir" 2>/dev/null || true

  # A primary checkout that is already open gets focused instead of duplicated.
  ws=$("$herdr" worktree list --cwd "$dir" 2>/dev/null |
    jq -r --arg d "$dir" '.result.worktrees[]? | select(.path == $d) | .open_workspace_id // empty' 2>/dev/null || true)
  if [ -n "$ws" ]; then
    call workspace focus "$ws" >/dev/null
  else
    call workspace create --cwd "$dir" --focus >/dev/null
  fi
}

cmd_agent() {
  [ $# -gt 0 ] || fail "usage: herdr-launch agent KIND..."
  local cwd common root kind default branch name out pane
  cwd=${HERDR_ACTIVE_PANE_CWD:-$PWD}

  # herdr only creates worktrees from the primary checkout, so resolve it
  # even when the active pane sits in a linked worktree.
  common=$(git -C "$cwd" rev-parse --path-format=absolute --git-common-dir 2>/dev/null) ||
    fail "not inside a git repository: $cwd"
  [ "$(basename "$common")" = .git ] || fail "unsupported repository layout: $common"
  root=$(dirname "$common")

  kind=$(printf '%s\n' "$@" | fzf --prompt="agent in $(basename "$root")> " --no-sort) || exit 0
  default="$kind/$(date +%m%d-%H%M)"
  read -rei "$default" -p 'branch: ' branch || exit 0
  branch=${branch:-$default}

  # Agent names must match [a-z][a-z0-9_-]{0,31}.
  name=$(printf '%s' "$branch" | tr '[:upper:]' '[:lower:]' | tr -c 'a-z0-9_-' '-')
  case $name in [a-z]*) ;; *) name="a$name" ;; esac
  name=${name:0:32}

  out=$(call worktree create --cwd "$root" --branch "$branch" --label "$branch" --focus)
  pane=$(jq -r '.result.root_pane.pane_id' <<<"$out")
  call agent start "$name" --kind "$kind" --pane "$pane" >/dev/null
}

case ${1:-} in
  project) shift; cmd_project "$@" ;;
  agent) shift; cmd_agent "$@" ;;
  *) fail "usage: herdr-launch {project|agent KIND...}" ;;
esac
