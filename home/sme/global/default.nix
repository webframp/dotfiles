{ inputs, lib, pkgs, config, outputs, ... }:

{
  nix = {
    package = lib.mkDefault pkgs.nix;
    settings = {
      experimental-features = [ "nix-command" "flakes" "repl-flake" ];
      warn-dirty = false;
    };
  };

  programs.home-manager.enable = true;

  home = {
    username = lib.mkDefault "sme";
    homeDirectory = lib.mkDefault "/home/${config.home.username}";
    stateVersion = lib.mkDefault "23.05";
  };

  # startup speed checking
  # for i in $(seq 1 5); do /run/current-system/sw/bin/time -p ~/.nix-profile/bin/zsh -i -c exit; done
  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableCompletion = true;
    enableSyntaxHighlighting = true;
    enableVteIntegration = true;
    autocd = true;
    history = {
      expireDuplicatesFirst = true;
      extended = true;
      ignoreDups = true;
      ignoreSpace = true;
    };
    initExtra = builtins.readFile ./includes/zshrc;
    loginExtra = builtins.readFile ./includes/zlogin;
    # https://nixos.wiki/wiki/Zsh#Zplug
    # https://nix-community.github.io/home-manager/options.html#opt-programs.zsh.zplug.enable
    # https://github.com/zplug/zplug#3-tags-for-zplug
    zplug = {
      enable = true;
      plugins = [
        # Plugins live in their own forks that are kept up to date.
        # I've been bitten by authors removing plugins from upstream before
        { name = "webframp/zsh-async"; }
        { name = "webframp/zsh-completions"; }
        {
          name = "webframp/powerlevel10k";
          tags = [ "as:theme" "depth:1" ];
        }
      ];
    };
  };

  # TODO these files need to exist but it's manual for now
  programs.keychain = {
    enable = true;
    enableZshIntegration = true;
    agents = [ "ssh" "gpg" ];
    keys = [ "id_ed25519" "BE06ADB38C7F719D" ];
  };

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.zoxide = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
    nix-direnv.enable = true;
  };

  programs.exa = { enable = true; };

  home.file.".gemrc".text = "gem: --no-ri --no-rdoc";
  home.file.".p10k.zsh".source = ./includes/p10k.zsh;

  home.sessionVariables = {
    TERM = "xterm-24bit";
    LESSCHARSET = "utf-8";
    # Colors for manpages
    LESS_TERMCAP_mb = "$'E[01;31m'";
    LESS_TERMCAP_md = "$'E[01;31m'";
    LESS_TERMCAP_me = "$'E[0m'";
    LESS_TERMCAP_se = "$'E[0m'";
    LESS_TERMCAP_so = "$'E[01;44;33m'";
    LESS_TERMCAP_ue = "$'E[0m'";
    LESS_TERMCAP_us = "$'E[01;32m'";
    BAT_THEME = "base16-256";
    ZSH_AUTOSUGGEST_USE_ASYNC = 1;
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
