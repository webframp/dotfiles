# ABOUTME: Runs `kiro-cli doctor` after activation to bootstrap its sidecar runtime
# ABOUTME: kiro-cli downloads bun/node/tui.js into Application Support on first run; Nix can't manage that
{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.webframp.kiroCli;
in {
  options.webframp.kiroCli = {
    enable = lib.mkEnableOption "kiro-cli sidecar runtime bootstrap";
  };

  config = lib.mkIf cfg.enable {
    # `doctor` (no flags) stops at the first unfixable check instead of
    # running the full suite, so it's only useful here for its fix side
    # effect (bootstrapping the sidecar runtime). `doctor --all` runs every
    # check with no fixes, which is what diagnostics needs. Three of its
    # checks are structurally unable to pass from a non-interactive
    # activation script regardless of real system state (a live
    # terminal-integration hook, a live terminal socket, and the CLI's own
    # Nix store path vs ~/.local/bin), so they're excluded outright.
    # Everything else is diffed against the previous run's failures: known
    # failures stay quiet, newly appearing ones get printed so a regression
    # introduced by a kiro-cli upgrade doesn't go unnoticed.
    home.activation.kiroCliDoctor = lib.hm.dag.entryAfter ["writeBoundary"] ''
      kiroCli="${pkgs.kiro-cli}/bin/kiro-cli"
      stateDir="$HOME/Library/Application Support/kiro-cli"
      baseline="$stateDir/doctor-baseline.txt"
      mkdir -p "$stateDir"

      PATH="$HOME/.local/bin:$PATH" "$kiroCli" doctor >/dev/null 2>&1 || true

      output=$(PATH="$HOME/.local/bin:$PATH" "$kiroCli" doctor --all 2>&1 || true)
      failures=$(printf '%s\n' "$output" \
        | grep '^✘' \
        | grep -v -e 'terminal integrations' -e 'Qterm Socket Check' -e 'Valid CLI path' \
        || true)

      if [ -f "$baseline" ]; then
        newFailures=$(comm -13 <(sort "$baseline") <(printf '%s\n' "$failures" | sort) || true)
        if [ -n "$newFailures" ]; then
          echo "kiro-cli doctor: new issue(s) since last activation:"
          printf '%s\n' "$newFailures"
        fi
      fi
      printf '%s\n' "$failures" > "$baseline"
    '';
  };
}
