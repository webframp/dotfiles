# ABOUTME: Shared home-manager configuration for non-bluestreak hosts
# ABOUTME: Used by ubuntu-wsl and generic linux configurations
{
  inputs,
  lib,
  pkgs,
  config,
  outputs,
  ...
}: {
  imports = with outputs.homeManagerModules; [
    zsh
    bat
    delta
    direnv
    fzf
    git
    tmux
  ];

  nix = {
    package = lib.mkDefault pkgs.nix;
    settings = {
      experimental-features = ["nix-command" "flakes"];
      warn-dirty = false;
    };
  };

  nixpkgs.config.allowUnfreePredicate = _: true;
  programs.home-manager.enable = true;

  home = {
    username = lib.mkDefault "sme";
    homeDirectory = lib.mkDefault "/home/${config.home.username}";
    stateVersion = lib.mkDefault "23.05";
    packages = import ../packages.nix {inherit lib pkgs;};
  };

  gtk = {enable = true;};

  # Shared module configuration
  webframp.bat.enable = true;
  webframp.delta.enable = true;
  webframp.direnv = {
    enable = true;
    whitelist = ["~/src/o11n"];
  };
  webframp.fzf.enable = true;
  webframp.git.enable = true;
  webframp.tmux = {
    enable = true;
    terminal = "tmux-256color";
  };

  # Zsh configuration via shared module
  # startup speed checking: for i in $(seq 1 5); do /run/current-system/sw/bin/time -p ~/.nix-profile/bin/zsh -i -c exit; done
  webframp.zsh = {
    enable = true;
    enableVterm = false; # Enable on hosts that use Emacs

    extraEnvVars = ''
      export FORCE_NO_ALIAS=true
    '';
  };

  programs.granted.enable = true;

  programs.zoxide.enable = true;

  programs.eza = {
    enable = true;
    # creates these aliases
    # ls = "eza";
    # ll = "eza -l";
    # la = "eza -a";
    # lt = "eza --tree";
    # lla = "eza -la";
  };

  home.file.".gemrc".text = "gem: --no-ri --no-rdoc";
  # p10k.zsh is managed by the webframp.zsh module

  home.file.".icons/default".source = "${pkgs.vanilla-dmz}/share/icons/Vanilla-DMZ";

  home.file.".xterm-24bit.terminfo" = {
    source = ./includes/xterm-24bit.terminfo;
    onChange = "tic -x -o ~/.terminfo ~/.xterm-24bit.terminfo";
  };

  # Ensure UTF-8
  home.language = {
    base = "en_US.UTF-8";
    ctype = "en_US.UTF-8";
    numeric = "en_US.UTF-8";
    time = "en_US.UTF-8";
    collate = "en_US.UTF-8";
    monetary = "en_US.UTF-8";
    messages = "en_US.UTF-8";
    name = "en_US.UTF-8";
  };
}
