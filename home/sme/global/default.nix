{ inputs, lib, pkgs, config, outputs, ... }:

{
  nix = {
    package = lib.mkDefault pkgs.nix;
    settings = {
      experimental-features = [ "nix-command" "flakes" "repl-flake" ];
      warn-dirty = false;
    };
  };

  nixpkgs.config.allowUnfreePredicate = _: true;
  programs.home-manager.enable = true;

  home = {
    username = lib.mkDefault "sme";
    homeDirectory = lib.mkDefault "/home/${config.home.username}";
    stateVersion = lib.mkDefault "23.05";
    packages = with pkgs; [
      awscli2
      azure-cli
      awslogs
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
      vanilla-dmz
      vault
      youtube-dl
      zbar
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

  gtk = { enable = true; };

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

  programs.bat = {
    enable = true;
    extraPackages = with pkgs.bat-extras; [ batdiff batman batgrep batwatch ];
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

  # programs.vscode = {
  #   enable = true;
  #   extensions = with pkgs.vscode-extensions;
  #     [
  #       asvetliakov.vscode-neovim
  #       bbenoist.nix
  #       davidanson.vscode-markdownlint
  #       eamodio.gitlens
  #       github.copilot
  #       ms-vscode.powershell
  #       ms-vscode-remote.remote-ssh
  #       streetsidesoftware.code-spell-checker
  #     ] ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
  #       {
  #         name = "copilot-chat";
  #         publisher = "github";
  #         version = "0.4.1";
  #         sha256 = "sha256-qoIkvuPB2Y5Rfpq0vt8/6MzZCBTHRVsIO9e58asWXgU=";
  #       }
  #       {
  #         name = "tokyo-night";
  #         publisher = "enkia";
  #         version = "1.0.0";
  #         sha256 = "sha256-/fM+aUDUzVJ6P38i+GrxhLv2eLJNa8OFkKsM4yPBy4c=";
  #       }
  #     ];
  # };

  home.file.".gemrc".text = "gem: --no-ri --no-rdoc";
  home.file.".p10k.zsh".source = ./includes/p10k.zsh;

  home.file.".icons/default".source =
    "${pkgs.vanilla-dmz}/share/icons/Vanilla-DMZ";

  home.file.".xterm-24bit.terminfo" = {
    source = ./includes/xterm-24bit.terminfo;
    onChange = "tic -x -o ~/.terminfo ~/.xterm-24bit.terminfo";
  };

  home.shellAliases = {
    # git
    gst = "git status";
    gpo = "git push origin HEAD";
    gpu = "git pull --prune --tags --all";
    repo = "git browse >/dev/null";

    # SSH
    ssh = "TERM=xterm-256color ssh";

    # exa replaces ls
    l = "exa";
    la = "exa -la";
    ll = "exa -lag";
    lg = "exa -bghHliS --git";
    tree = "exa --tree";

    # nicer man pages
    man = "batman";

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
