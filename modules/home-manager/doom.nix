# ABOUTME: Doom Emacs config symlink via mkOutOfStoreSymlink
# ABOUTME: Points ~/.config/doom to the dotfiles working copy, keeping files writable
{
  config,
  lib,
  ...
}: let
  cfg = config.webframp.doom;
in {
  options.webframp.doom = {
    enable = lib.mkEnableOption "Doom Emacs config symlink";
  };

  config = lib.mkIf cfg.enable {
    xdg.configFile."doom".source =
      config.lib.file.mkOutOfStoreSymlink
        "${config.home.homeDirectory}/src/webframp/dotfiles/config/doom";
  };
}
