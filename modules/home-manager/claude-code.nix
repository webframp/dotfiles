# ABOUTME: Claude Code settings.json symlink via a direct activation-script symlink
# ABOUTME: Points ~/.claude/settings.json to the dotfiles working copy, keeping it writable
#
# Deliberately NOT home.file + mkOutOfStoreSymlink: that indirects through the
# home-files store profile, producing a double-hop symlink
# (~/.claude/settings.json -> store/home-files/... -> store/hm_settings.json ->
# dotfiles). Claude Code's atomic settings writer (write .tmp, rename) only
# resolves one symlink hop when locating the .tmp file's directory, lands in
# the read-only store, and fails with EROFS. A single direct symlink avoids
# the extra hop.
{
  config,
  lib,
  ...
}: let
  cfg = config.webframp.claudeCode;
  target = "${config.home.homeDirectory}/src/webframp/dotfiles/config/claude/settings.json";
in {
  options.webframp.claudeCode = {
    enable = lib.mkEnableOption "Claude Code settings.json symlink";
  };

  config = lib.mkIf cfg.enable {
    home.activation.linkClaudeSettings = lib.hm.dag.entryAfter ["writeBoundary"] ''
      run mkdir -p "$HOME/.claude"
      run ln -sfn "${target}" "$HOME/.claude/settings.json"
    '';

    # settings.json's statusLine points at this stable, machine-portable
    # path (~/.claude/claude-code-status/statusline.sh) rather than the
    # versioned plugin cache path, which changes on every plugin update.
    home.file.".claude/claude-code-status/statusline.sh" = {
      source = ../../home/sme/shared/includes/claude-code-status-dispatcher.sh;
      executable = true;
    };
  };
}
