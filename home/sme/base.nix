# ABOUTME: Base home-manager configuration shared across all platforms
# ABOUTME: Provides common module imports and webframp module defaults
{
  lib,
  pkgs,
  outputs,
  ...
}: {
  imports =
    [
      ./programs.nix
    ]
    ++ (with outputs.homeManagerModules; [
      zsh
      bat
      delta
      direnv
      fzf
      git
      tmux
    ]);

  nix = {
    package = lib.mkDefault pkgs.nix;
    settings = {
      experimental-features = ["nix-command" "flakes"];
      warn-dirty = false;
    };
  };

  nixpkgs.config.allowUnfreePredicate = _: true;
  programs.home-manager.enable = true;

  # Shared webframp module configuration
  # Hosts can override specific options as needed
  webframp.bat.enable = true;
  webframp.delta.enable = true;
  webframp.direnv = {
    enable = true;
    whitelist = ["~/src/o11n"];
  };
  webframp.fzf.enable = true;
  webframp.git.enable = true;
  webframp.tmux.enable = true;
  webframp.zsh.enable = true;
}
