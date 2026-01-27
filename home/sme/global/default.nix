# ABOUTME: Shared home-manager configuration for non-bluestreak hosts
# ABOUTME: Used by ubuntu-wsl and generic linux configurations
{
  lib,
  pkgs,
  config,
  ...
}: {
  imports = [../base.nix];

  home = {
    username = lib.mkDefault "sme";
    homeDirectory = lib.mkDefault "/home/${config.home.username}";
    stateVersion = lib.mkDefault "23.05";
    packages = import ../packages.nix {inherit lib pkgs;};
  };

  # Linux-specific webframp overrides
  webframp.tmux.terminal = "tmux-256color";

  webframp.zsh = {
    enableVterm = false; # Enable on hosts that use Emacs
    extraEnvVars = ''
      export FORCE_NO_ALIAS=true
    '';
  };
}
