# This replaces /etc/nixos/configuration.nix

{ inputs, outputs, lib, config, pkgs, modulesPath, ... }: {
  # You can import other NixOS modules here
  imports = [
    # If you want to use modules from other flakes (such as nixos-hardware):
    # inputs.hardware.nixosModules.common-cpu-amd
    # inputs.hardware.nixosModules.common-ssd
    "${modulesPath}/profiles/minimal.nix"
    inputs.nixos-wsl.nixosModules.wsl

    # Import home-manager module
    inputs.home-manager.nixosModules.home-manager

    # You can also split up your configuration and import pieces of it here:
    # ./users.nix

    # Import your generated (nixos-generate-config) hardware configuration
    ./hardware-configuration.nix

    ../common/users/sme
  ];

  wsl = {
    enable = true;
    wslConf.automount.root = "/mnt";
    defaultUser = "sme";
    startMenuLaunchers = true;

    # Enable native Docker support
    # docker-native.enable = true;

    # Enable integration with Docker Desktop (needs to be installed)
    # docker-desktop.enable = true;

  };

  nixpkgs = {
    # You can add overlays here
    overlays = [
      # Add overlays your own flake exports (from overlays and pkgs dir):
      outputs.overlays.additions
      outputs.overlays.modifications
      outputs.overlays.unstable-packages

      # If you want to use overlays exported from other flakes:
      # neovim-nightly-overlay.overlays.default

      # Or define it inline, for example:
      # (final: prev: {
      #   hi = final.hello.overrideAttrs (oldAttrs: {
      #     patches = [ ./change-hello-to-hi.patch ];
      #   });
      # })
    ];
    # Configure your nixpkgs instance
    config = {
      # Disable if you don't want unfree packages
      allowUnfree = true;
    };
  };

  nix = {
    # This will add each flake input as a registry
    # To make nix3 commands consistent with your flake
    registry = lib.mapAttrs (_: value: { flake = value; }) inputs;

    # This will additionally add your inputs to the system's legacy channels
    # Making legacy nix commands consistent as well, awesome!
    nixPath = lib.mapAttrsToList (key: value: "${key}=${value.to.path}")
      config.nix.registry;

    gc = {
      automatic = true;
      persistent = true;
      dates = "weekly";
    };

    settings = {
      # Enable flakes and new 'nix' command
      experimental-features = "nix-command flakes";
      # Deduplicate and optimize nix store
      auto-optimise-store = true;
      trusted-users = [ "root" "sme" ];

      # Binary cache setup
      # Run "cachix use <repo>" take values from ~/.config/nix/nix.conf
      # Then remove generated conf file
      substituters =
        [ "https://nix-community.cachix.org" "https://cache.nixos.org" ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      ];
    };

  };

  networking.hostName = "galvatron-wsl";
  environment.noXlibs = false;

  environment.systemPackages = with pkgs; [
    man-pages
    man-pages-posix

    awscli2
    azure-cli
    bat
    browserpass
    coreutils
    delta
    dig
    exa
    fd
    file
    fzf
    git
    git-lfs
    gnupg
    htop
    inetutils
    ispell
    jq
    yq-go
    keychain
    mob
    neofetch
    (pass.withExtensions
      (ext: with ext; [ pass-genphrase pass-otp pass-update ]))
    pass-git-helper
    pinentry
    pinentry-curses
    ripgrep
    speedtest-cli
    tree
    urlview
    wget
    unzip
    vault
    zip
    zoxide
    tmux
    emacs-nox
    vault
    # Instead of: sudo ln -s /mnt/c/WINDOWS/system32/clip.exe /usr/bin/wl-copy
    wl-clipboard

    podman
    podman-compose

    # kubernetes
    k9s
    kubectl
    kubectx
    kubernetes-helm
    go
    clang
    nodejs
    nodePackages.npm
    nodePackages.pnpm

    # For various emacs modes
    sqlite
    wordnet
    editorconfig-checker
    editorconfig-core-c
    gopls
    gomodifytags
    gotests
    gore
    gotools
    multimarkdown
    nixfmt
    pandoc
    shfmt
    shellcheck
    # TODO how do I get rid of needing to write nodePackages here?
    nodePackages.vscode-json-languageserver-bin
    nodePackages.bash-language-server

    # Fonts
    inconsolata-nerdfont

    # Super lightweight browser, small install, fast launch for testing
    dillo
    st
    firefox

    # Nix related
    cachix
    direnv
    nix-index
    nox
    patchelf

    # custom packages
    granted
    # afterwards run: (these should be automatic somehow)
    # granted browser set -b firefox -p /mnt/c/Users/sme/scoop/shims/firefox.exe (if using non-wsl ff)
    # granted browser set-sso -b firefox

    # TODO how to enable home-manager as a module to manage user dotfiles?
  ]; # ++ [ (import ./pkg-granted.nix) ];

  environment.shellAliases = {
    # clipboard stuff
    pbpaste = "powershell.exe -noprofile Get-Clipboard";
    pbcopy = "clip.exe";

    l = "exa";
    la = "exa -la";
    ll = "exa -lag";
    lg = "exa -bghHliS --git";
    tree = "exa --tree";

    reload = "exec $SHELL -l";

    # git
    gst = "git status";
    gpo = "git push origin HEAD";
    gpu = "git pull --prune --tags --all";

    # doom
    doom = "~/.config/emacs/bin/doom";

    # kube
    k = "kubectl";
    kx = "kubectx";
    kn = "kubens";

    # updates
    yay = "sudo nixos-rebuild switch --flake .#galvatron";

    # granted.dev tools
    assume = "source assume";

    # local term uses 24bit color terminfo, remote does not
    ssh = "TERM=xterm-256color ssh";
  };

  programs.bash = {
    enableCompletion = true;
    enableLsColors = true;
    interactiveShellInit = ''
      # Ensure UTF-8
      export LC_ALL=en_US.UTF-8
      export LANG=en_US.UTF-8
      export LANGUAGE=en_US.UTF-8

      # Colors for manpages
      export LESS_TERMCAP_mb=$'\E[01;31m'
      export LESS_TERMCAP_md=$'\E[01;31m'
      export LESS_TERMCAP_me=$'\E[0m'
      export LESS_TERMCAP_se=$'\E[0m'
      export LESS_TERMCAP_so=$'\E[01;44;33m'
      export LESS_TERMCAP_ue=$'\E[0m'
      export LESS_TERMCAP_us=$'\E[01;32m'

      # This requires xterm-24bit.terminfo file and
      # running the command: tic -x -o ~/.terminfo xterm-24bit.terminfo
      # no idea yet how to integrate this into nixos
      export TERM=xterm-24bit

      # maybe if ssh breaks
      # alias ssh="TERM=xterm-256color ssh"

      # remove agents arg if using only gpg
      # BE06ADB38C7F719D must exist, not managing gpg setup with nix yet
      # after copying .gnupg from encrypted backup, run:
      # chown -R $(whoami) ~/.gnupg/
      # find ~/.gnupg -type f -exec chmod 600 {} \;
      # find ~/.gnupg -type d -exec chmod 700 {} \;
      eval $(keychain --eval --agents ssh,gpg id_ed25519 BE06ADB38C7F719D)

      eval "$(zoxide init bash)"
      eval "$(direnv hook bash)"

      # source private functions if provided
      [ -f ~/.profile.d/bethel.sh ] && source ~/.profile.d/bethel.sh
    '';
  };

  # A basic neovim config as a backup. I don't use it much
  programs.neovim = {
    enable = true;

    viAlias = true;
    vimAlias = true;

    configure = {
      customRC = ''
        let mapleader = " "
        colors onedarkpro
        filetype plugin indent on
        set tabstop=4
        set shiftwidth=4
        set expandtab
        set number
        set list
        set timeoutlen=500
        "set listchars=tab:→\ ,space:·,nbsp:␣,trail:•,eol:¶,precedes:«,extends:»
        set listchars=tab:→\ ,space:·,nbsp:␣,trail:•,precedes:«,extends:»
        autocmd FileType nix setlocal commentstring=#\ %s
      '';
      #    set cc=80
      #    if &diff
      #      colorscheme blue
      #    endif

      packages.myVimPackage = with pkgs.vimPlugins; {
        start = [
          plenary-nvim
          neo-tree-nvim
          neovim-sensible
          nvim-autopairs
          nvim-colorizer-lua
          nvim-treesitter.withAllGrammars
          nvim-web-devicons
          onedarkpro-nvim
          telescope-nvim
          todo-comments-nvim
          vim-commentary
          vim-airline
          vim-airline-themes
          vim-better-whitespace
          vim-fugitive
          vim-repeat
          vim-shellcheck
          vim-surround
          which-key-nvim
          zoxide-vim
        ];
      };
    };
  };

  programs.bash-my-aws.enable = true;
  programs.browserpass.enable = true;
  programs.firefox.nativeMessagingHosts.browserpass = true;

  # TODO
  # user.defaultUserShell = pkgs.zsh;
  programs.zsh = {
    enable = true;
    # https://mynixos.com/help/home-manager
    # https://nixos.wiki/wiki/Zsh
    # need to setup home-manager?
  };

  programs.tmux = {
    enable = true;
    shortcut = "j";
    # aggressiveResize = true; -- Disabled to be iTerm-friendly
    baseIndex = 1;
    # Stop tmux+escape printing nonsense
    # https://github.com/tmux-plugins/tmux-sensible/issues/61
    escapeTime = 1;

    newSession = false;
    # Force tmux to use /tmp for sockets (WSL2 compat)
    secureSocket = false;
    clock24 = true;
    plugins = with pkgs; [
      tmuxPlugins.better-mouse-mode
      tmuxPlugins.continuum
      tmuxPlugins.extrakto # prefix + tab
      tmuxPlugins.fzf-tmux-url
      tmuxPlugins.onedark-theme
      tmuxPlugins.pain-control
      tmuxPlugins.resurrect
      tmuxPlugins.sensible
      tmuxPlugins.tmux-thumbs # prefix + I
      tmuxPlugins.yank
    ];

    extraConfig = ''
      # Use 24bit colors if possible or fallback to 256
      # https://old.reddit.com/r/tmux/comments/mesrci/tmux_2_doesnt_seem_to_use_256_colors/
      # https://github.com/syl20bnr/spacemacs/wiki/Terminal

      if -b 'test $TERM = xterm-24bit' 'set -g default-terminal "xterm-24bit"' 'set -g default-terminal "xterm-256color"'
      if -b 'test $TERM = xterm-24bit' 'set -g terminal-overrides ",xterm-24bit:Tc"' 'set -ga terminal-overrides ",*256col*:Tc"'
      if -b 'test $TERM = xterm-24bit' 'set -ga terminal-overrides "*:Ss=\E[%p1%d q:Se=\E[ q"'
      if -b 'test $TERM = xterm-24bit' 'set-environment -g COLORTERM "truecolor"'

      # Mouse enabled
      set-option -g mouse on

      # copy to X11 clipboard
      if -b 'command -v xsel > /dev/null 2>&1' 'bind y run -b "tmux save-buffer - | xsel -i -b"'
      if -b '! command -v xsel > /dev/null 2>&1 && command -v xclip > /dev/null 2>&1' 'bind y run -b "tmux save-buffer - | xclip -i -selection clipboard >/dev/null 2>&1"'
      # copy to Wayland clipboard
      if -b 'command -v wl-copy > /dev/null 2>&1' 'bind y run -b "tmux save-buffer - | wl-copy"'
      # copy to macOS clipboard
      if -b 'command -v pbcopy > /dev/null 2>&1' 'bind y run -b "tmux save-buffer - | pbcopy"'
      if -b 'command -v reattach-to-user-namespace > /dev/null 2>&1' 'bind y run -b "tmux save-buffer - | reattach-to-user-namespace pbcopy"'
      # copy to Windows clipboard
      if -b 'command -v clip.exe > /dev/null 2>&1' 'bind y run -b "tmux save-buffer - | clip.exe"'
      if -b '[ -c /dev/clipboard ]' 'bind y run -b "tmux save-buffer - > /dev/clipboard"'

      # buffers
      bind b list-buffers     # list paste buffers
      bind p paste-buffer -p  # paste from the top paste buffer
      bind P choose-buffer    # choose which buffer to paste from

      bind -r C-h previous-window # select previous window
      bind -r C-l next-window     # select next window
    '';
  };

  # Also include man pages for system docs
  documentation.enable = true;
  documentation.man.enable = true;
  documentation.nixos.enable = true;
  documentation.dev.enable = true;

  # Fix for https://github.com/microsoft/WSL/issues/9303
  #systemd.services.fix-ro-mount-tmp = {
  #  script = ''
  #    /run/wrappers/bin/mount -o remount,rw /tmp/.X11-unix/
  #  '';
  #  wantedBy = [ "multi-user.target" ];
  #};

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  system.stateVersion = "22.11";
}
