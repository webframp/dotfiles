# ABOUTME: Shared home-manager configuration for non-bluestreak Linux hosts
# ABOUTME: Used by galvatron-wsl, ubuntu-wsl, and generic linux configurations
{
  lib,
  config,
  ...
}: {
  imports = [../base.nix];

  home = {
    username = lib.mkDefault "sme";
    homeDirectory = lib.mkDefault "/home/${config.home.username}";
    stateVersion = lib.mkDefault "23.05";
  };

  # Linux-specific zsh extras (tmux.terminal and zsh.enableVterm are in base.nix)
  webframp.zsh.extraEnvVars = ''
    export FORCE_NO_ALIAS=true
  '';
}
