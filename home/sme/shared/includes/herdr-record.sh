# ABOUTME: herdr popup helper: start an asciinema-recorded shell in a new tab
# ABOUTME: Packaged by modules/home-manager/herdr.nix; run from a [[keys.command]] popup
#
#   herdr-record start   prompt for a title, open a new tab in the active
#                        pane's directory, and start `asciinema rec` wrapping
#                        an interactive shell there. The cast is written to
#                        HERDR_RECORDINGS_DIR as <timestamp>-<slug>.cast.
#                        Exit the shell (type `exit`) to finalize the cast.
#
# The recording is generic: whatever you do in that shell is captured, so the
# same binding records a datastore-setup session, extension work, or anything
# else. Render a cast to MP4 with the @webframp/screencast swamp behavior.

herdr=${HERDR_BIN_PATH:-herdr}
recordings_dir=${HERDR_RECORDINGS_DIR:-$HOME/recordings}
asciinema=${HERDR_ASCIINEMA_BIN:-asciinema}

# A leading ~ in the configured dir is literal until we expand it here.
recordings_dir=${recordings_dir/#\~/$HOME}

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

cmd_start() {
  command -v "$asciinema" >/dev/null 2>&1 ||
    fail "asciinema not found on PATH. Install it (home-manager) before recording."

  local title slug stamp cast cwd out pane
  read -rei "session" -p 'recording title: ' title || exit 0
  title=${title:-session}

  # Slug: lowercase, non-alphanumerics to single dashes, trimmed.
  slug=$(printf '%s' "$title" | tr '[:upper:]' '[:lower:]' |
    tr -c 'a-z0-9' '-' | tr -s '-' | sed 's/^-//; s/-$//')
  [ -n "$slug" ] || slug=session
  stamp=$(date +%Y%m%d-%H%M%S)
  mkdir -p "$recordings_dir" || fail "cannot create $recordings_dir"
  cast="$recordings_dir/$stamp-$slug.cast"

  cwd=${HERDR_ACTIVE_PANE_CWD:-$PWD}

  out=$(call tab create --cwd "$cwd" --label "rec:$slug" --focus)
  pane=$(jq -r '.result.root_pane.pane_id // empty' <<<"$out")
  [ -n "$pane" ] || fail "herdr tab create returned no pane id"

  # `herdr pane run` joins its COMMAND tokens with spaces and sends the result
  # into the pane's shell as typed text, so any argument with spaces (the title)
  # must already be shell-quoted in that one string. Build the command line with
  # printf %q and hand it to pane run as a single argument.
  local cmdline
  cmdline=$(printf '%q ' "$asciinema" rec --title "$title" --overwrite "$cast")
  cmdline=${cmdline% } # drop the trailing space

  # asciinema rec with no command records $SHELL; --title stamps the cast
  # header so the swamp behavior's `list` can surface it. --overwrite is safe:
  # the timestamped name is unique per second.
  call pane run "$pane" "$cmdline" >/dev/null

  "$herdr" notification show "Recording to $cast (exit the shell to finish)" >/dev/null 2>&1 || true
}

case ${1:-} in
  start) shift; cmd_start "$@" ;;
  *) fail "usage: herdr-record start" ;;
esac
