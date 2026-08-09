# ABOUTME: Claude Code settings.json symlink via mkOutOfStoreSymlink
# ABOUTME: Points ~/.claude/settings.json to the dotfiles working copy, keeping it writable
{
  config,
  lib,
  ...
}: let
  cfg = config.webframp.claudeCode;
in {
  options.webframp.claudeCode = {
    enable = lib.mkEnableOption "Claude Code settings.json symlink";
  };

  config = lib.mkIf cfg.enable {
    home.file.".claude/settings.json".source =
      config.lib.file.mkOutOfStoreSymlink
        "${config.home.homeDirectory}/src/webframp/dotfiles/config/claude/settings.json";
  };
}
