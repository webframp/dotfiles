# This replaces /etc/nixos/configuration.nix
{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  modulesPath,
  ...
}: {
  # You can import other NixOS modules here
  imports = [
    "${modulesPath}/profiles/minimal.nix"
    inputs.nixos-wsl.nixosModules.wsl

    # Import home-manager module
    inputs.home-manager.nixosModules.home-manager

    # Generated (nixos-generate-config) hardware configuration
    ./hardware-configuration.nix

    ../common/users/sme
  ];

  wsl = {
    enable = true;
    wslConf.automount.root = "/mnt";
    defaultUser = "sme";
    startMenuLaunchers = true;
  };

  nixpkgs = {
    overlays = [
      outputs.overlays.additions
      outputs.overlays.modifications
    ];
    config = {allowUnfree = true;};
  };

  nix = {
    registry = lib.mapAttrs (_: value: {flake = value;}) inputs;
    # make legacy nix commands consistent
    nixPath =
      lib.mapAttrsToList (key: value: "${key}=${value.to.path}")
      config.nix.registry;

    gc = {
      automatic = true;
      persistent = true;
      dates = "weekly";
    };

    settings = {
      # enable flakes + nix
      experimental-features = "nix-command flakes";
      auto-optimise-store = true;
      trusted-users = ["root" "sme"];

      # Binary cache setup
      # Run "cachix use <repo>" take values from ~/.config/nix/nix.conf
      # Then remove generated conf file
      substituters = ["https://nix-community.cachix.org" "https://cache.nixos.org"];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      ];
    };
  };

  networking.hostName = "galvatron-wsl";

  programs.nix-ld.enable = true;
  programs.dconf.enable = true;
  environment.noXlibs = false;
  environment.systemPackages = with pkgs; [
    man-pages
    man-pages-posix

    (pass.withExtensions
      (ext: with ext; [pass-genphrase pass-otp pass-update]))
    pass-git-helper
    pinentry
    pinentry-curses
    p4
    speedtest-cli
    wsl-open
    wslu
    xdg-utils
    termdown
    emacs29-nox
    podman
    podman-compose

    # gui stuff
    material-icons
    shared-mime-info
    super-tiny-icons

    # kubernetes
    k9s
    kubectl
    kubectx
    kubernetes-helm
    kustomize
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
    rnix-lsp
    pandoc
    shfmt
    shellcheck
    # TODO how do I get rid of needing to write nodePackages here?
    nodePackages.vscode-json-languageserver-bin
    nodePackages.bash-language-server
    nodePackages.cspell
    python3Full
    dotnet-runtime_7
    dotnet-sdk_7
    vale

    # Super lightweight browser, small install, fast launch for testing
    dillo
    st
    firefox
    gnome.gnome-tweaks

    # Nix related
    cachix
    direnv
    nix-index
    nox
    patchelf

    # afterwards run: (these should be automatic somehow)
    # granted browser set -b firefox -p /mnt/c/Users/sme/scoop/shims/firefox.exe (if using non-wsl ff)
    # granted browser set-sso -b firefox
    risor

    # custom packages
    iamlive
  ];

  fonts = {
    fontconfig.enable = true;
    packages = with pkgs; [
      (nerdfonts.override {
        fonts = [
          "DroidSansMono"
          "FiraCode"
          "Hack"
          "Inconsolata"
          "Iosevka"
          "JetBrainsMono"
        ];
      })
    ];
  };

  users.users.sme.shell = pkgs.zsh;

  environment.shells = with pkgs; [zsh];
  environment.pathsToLink = ["/share/zsh"];
  environment.variables = {
    MOZ_ENABLE_WAYLAND = "1";
    LESSCHARSET = "utf-8";

    # Mainly for Emacs 28
    # https://github.com/emacs-mirror/emacs/blob/master/etc/NEWS.28#L179-L183
    COLORTERM = "truecolor";

    # BAT_THEME = "base16-256";

    # nix-ld https://nixos.wiki/wiki/Visual_Studio_Code#nix-ld
    NIX_LD_LIBRARY_PATH =
      lib.mkDefault (lib.makeLibraryPath [pkgs.stdenv.cc.cc]);
    NIX_LD =
      lib.mkDefault
      (lib.fileContents "${pkgs.stdenv.cc}/nix-support/dynamic-linker");

    # Moved these to shell init
    # Colors for manpages
    # LESS_TERMCAP_mb = "$'E[01;31m'";
    # LESS_TERMCAP_md = "$'E[01;31m'";
    # LESS_TERMCAP_me = "$'E[0m'";
    # LESS_TERMCAP_se = "$'E[0m'";
    # LESS_TERMCAP_so = "$'E[01;44;33m'";
    # LESS_TERMCAP_ue = "$'E[0m'";
    # LESS_TERMCAP_us = "$'E[01;32m'";
  };

  environment.shellAliases = {
    # clipboard stuff
    pbpaste = "powershell.exe -noprofile Get-Clipboard";
    pbcopy = "clip.exe";

    # doom
    doom = "~/.config/emacs/bin/doom";

    # kube
    k = "kubectl";
    kx = "kubectx";
    kn = "kubens";

    # local term uses 24bit color terminfo, remote does not
    ssh = "TERM=xterm-256color ssh";
  };

  programs.bash = {
    enableCompletion = true;
    enableLsColors = true;
    interactiveShellInit = ''
      # This requires xterm-24bit.terminfo file and
      # running the command: tic -x -o ~/.terminfo xterm-24bit.terminfo
      # no idea yet how to integrate this into nixos
      # https://github.com/NixOS/nixpkgs/blob/master/pkgs/tools/misc/mtm/default.nix#L24
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
  # maybe look at https://github.com/jamespwilliams/neovim-go-nix-develop/tree/main
  programs.neovim = {
    enable = true;

    viAlias = true;
    vimAlias = true;

    configure = {
      customRC = ''
        colors tokyonight-night
        filetype plugin indent on
        nnoremap <SPACE> <Nop>
        let mapleader = " "
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
          lightspeed-nvim
          plenary-nvim
          neo-tree-nvim
          neovim-sensible
          nvim-autopairs
          nvim-colorizer-lua
          nvim-treesitter.withAllGrammars
          nvim-web-devicons
          nvim-lspconfig
          (nvim-treesitter.withPlugins
            (plugins: with plugins; [tree-sitter-go]))
          tokyonight-nvim
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

  programs.zsh = {enable = true;};

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

  virtualisation = {
    podman = {
      enable = true;
      dockerCompat = true;
      defaultNetwork.settings = {dns_enabled = true;};
    };
  };

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  system.stateVersion = "23.05";
}
