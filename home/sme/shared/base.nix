# ABOUTME: Base home-manager configuration shared across all platforms
# ABOUTME: Provides common module imports, packages, and webframp module defaults
{
  config,
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

  # Note: nixpkgs.config is set per-host for standalone home-manager configs.
  # NixOS hosts using useGlobalPkgs inherit nixpkgs config from NixOS.
  programs.home-manager.enable = true;

  # Home defaults with platform-appropriate paths
  home = {
    username = lib.mkDefault "sme";
    homeDirectory = lib.mkDefault (
      if pkgs.stdenv.isDarwin
      then "/Users/${config.home.username}"
      else "/home/${config.home.username}"
    );
    stateVersion = lib.mkDefault "24.05";
    # Common packages for all platforms (hosts can add more via home.packages)
    packages = import ./packages.nix {inherit lib pkgs;};
  };

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

  # Linux-specific defaults (can be overridden by hosts)
  webframp.tmux.terminal = lib.mkIf pkgs.stdenv.isLinux "tmux-256color";
  webframp.zsh.enableVterm = lib.mkIf pkgs.stdenv.isLinux (lib.mkDefault false);
}
