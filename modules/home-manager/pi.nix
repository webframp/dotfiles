# ABOUTME: pi coding agent extensions and settings via mkOutOfStoreSymlink
# ABOUTME: Points ~/.pi/agent/{extensions,settings.json} at the dotfiles working copy, keeping it writable
{
  config,
  lib,
  ...
}: let
  cfg = config.webframp.pi;
in {
  options.webframp.pi = {
    enable = lib.mkEnableOption "pi coding agent extensions and settings symlink";
  };

  config = lib.mkIf cfg.enable {
    home.file.".pi/agent/extensions/status-line.ts".source =
      config.lib.file.mkOutOfStoreSymlink
        "${config.home.homeDirectory}/src/webframp/dotfiles/config/pi/extensions/status-line.ts";

    home.file.".pi/agent/settings.json".source =
      config.lib.file.mkOutOfStoreSymlink
        "${config.home.homeDirectory}/src/webframp/dotfiles/config/pi/settings.json";
  };
}
