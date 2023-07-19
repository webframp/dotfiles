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
    packages = with pkgs; [
      awscli2
      azure-cli
      awslogs
      bat
      coreutils
      csvkit
      delta
      dig
      exa
      fd
      file
      fzf
      git
      git-lfs
      git-extras
      glibcLocales
      gnupg
      htop
      inetutils
      ispell
      jq
      yq-go
      keychain
      mob
      neofetch
      onefetch
      pry
      ripgrep
      tmux
      tree
      urlview
      # Instead of: sudo ln -s /mnt/c/WINDOWS/system32/clip.exe /usr/bin/wl-copy
      wl-clipboard
      wget
      unzip
      vault
      zip
      zoxide

      # Nix related
      cachix
      direnv
      nix-index
      nox
      patchelf
    ];
  };

  # startup speed checking
  # for i in $(seq 1 5); do /run/current-system/sw/bin/time -p ~/.nix-profile/bin/zsh -i -c exit; done
  programs.zsh = {
    enable = true;
    enableCompletion = true;
    enableVteIntegration = true;
    autocd = true;
    history = {
      expireDuplicatesFirst = true;
      extended = true;
      ignoreDups = true;
      ignoreSpace = true;
    };
    envExtra = ''
      export TERM=xterm-24bit
      export ZSH_AUTOSUGGEST_USE_ASYNC=true;
    '';

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
        {
          name = "webframp/zsh-completions";
          tags = [ "defer:0" ];
        }
        {
          name = "webframp/zsh-autosuggestions";
          tags = [ "defer:2" "on:'webframp/zsh-completions'" ];
        }
        {
          name = "webframp/fast-syntax-highlighting";
          tags = [ "defer:3" "on:'webframp/zsh-autosuggestions'" ];
        }
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

  home.shellAliases = {
    # git
    gst = "git status";
    gpo = "git push origin HEAD";
    gpu = "git pull --prune --tags --all";
    repo = "git browse >/dev/null";

    # exa replaces ls
    l = "exa";
    la = "exa -la";
    ll = "exa -lag";
    lg = "exa -bghHliS --git";
    tree = "exa --tree";

    reload = "exec $SHELL -l";
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
