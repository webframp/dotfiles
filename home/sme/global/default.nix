{
  inputs,
  lib,
  pkgs,
  config,
  outputs,
  ...
}: let
  # Custom tmux theme
  # Would like to move this to a separate file somehow
  tmux-tokyo-night = pkgs.tmuxPlugins.mkTmuxPlugin {
    pluginName = "tmux-tokyo-night";
    version = "0.0.1";
    src = pkgs.fetchFromGitHub {
      owner = "webframp";
      repo = "tmux-tokyo-night";
      rev = "156a5a010928ebae45f0d26c3af172e0425fdda8";
      hash = "sha256-tANO0EyXiplXPitLrwfyOEliHUZkCzDJ6nRjEVps180=";
    };
    rtpFilePath = "tmux-tokyo-night.tmux";
  };
in {
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
    packages = with pkgs;
    with nodePackages_latest;
    with tflint-plugins; [
      ansible
      awscli2
      ssm-session-manager-plugin
      awslogs
      aws-vault
      aws-cdk
      nodePackages.cdktf-cli #_latest is broken
      cdk8s-cli
      claude-code
      coder
      coreutils
      csharpier
      csvkit
      delta
      devbox
      dig
      dockfmt
      dotnet-sdk
      fd
      ffmpeg
      file
      fzf
      gemini-cli
      gh
      git
      git-lfs
      git-extras
      # gitmux
      glab
      gnumake
      gnupg
      graphviz-nox
      html-tidy
      htop
      hugo
      inetutils
      ispell
      jq
      jsbeautifier
      yq-go
      keychain
      mob
      nodejs_20
      nomad
      pry
      python3
      rain
      ripgrep
      ruff
      terraform
      terraform-docs
      terraform-ls
      tflint
      tflint-ruleset-aws
      tfsec
      typescript-language-server
      stylelint
      urlscan
      # Instead of: sudo ln -s /mnt/c/WINDOWS/system32/clip.exe /usr/bin/wl-copy
      # wl-clipboard
      wget
      unzip
      uv
      vanilla-dmz
      vault
      xvfb-run
      xorg.xvfb
      yt-dlp
      # zbar
      zip
      zoxide

      # Nix related
      alejandra
      cachix
      direnv
      nix-index

      nox
      patchelf

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
      nil
      openssl
      pandoc
      shfmt
      shellcheck
      vale
      yaml-language-server
      vscode-langservers-extracted
      bash-language-server
      cspell
      prettier

      # zplug seems to need
      perl
    ];
  };

  gtk = {enable = true;};

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
      size = 100000;
      save = 100000;
    };

    # sessionVariables = {
    #   GRANTED_ALIAS_CONFIGURED = true;
    #   GRANTED_ENABLE_AUTO_REASSUME = false;
    #   GRANTED_QUIET = true;
    # };

    envExtra = ''
      export ZSH_AUTOSUGGEST_USE_ASYNC=true;
      export FORCE_NO_ALIAS=true
      export JSII_SILENCE_WARNING_UNTESTED_NODE_VERSION=true
    '';
    # alias assume="source ${pkgs.granted}/bin/.assume-wrapped"

    initContent = builtins.readFile ./includes/zshrc;
    loginExtra = builtins.readFile ./includes/zlogin;
    profileExtra = ''
      WORDCHARS=''${WORDCHARS//\/[&.;]}                                 # Don't consider certain characters part of the word
    '';
    # https://nixos.wiki/wiki/Zsh#Zplug
    # https://nix-community.github.io/home-manager/options.html#opt-programs.zsh.zplug.enable
    # https://github.com/zplug/zplug#3-tags-for-zplug
    zplug = {
      enable = true;
      plugins = [
        # Plugins live in their own forks that are kept up to date.
        # I've been bitten by authors removing plugins from upstream before
        {name = "webframp/zsh-async";}
        {
          name = "webframp/zsh-completions";
          tags = ["defer:0"];
        }
        {
          name = "webframp/zsh-autosuggestions";
          tags = ["defer:2" "on:'webframp/zsh-completions'"];
        }
        {
          name = "webframp/fast-syntax-highlighting";
          tags = ["defer:3" "on:'webframp/zsh-autosuggestions'"];
        }
        {
          name = "webframp/powerlevel10k";
          tags = ["as:theme" "depth:1"];
        }
      ];
    };
  };

  programs.bat = {
    enable = true;
    extraPackages = with pkgs.bat-extras; [batman batgrep batwatch];
  };

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
    # CTRL-T
    fileWidgetOptions = [
      "--preview 'bat -n --color=always {}'"
      " --bind 'ctrl-/:change-preview-window(down|hidden|)'"
    ];
    # CTRL-R
    historyWidgetOptions = [
      "--preview 'echo {}'"
      "--preview-window up:3:hidden:wrap"
      "--bind 'ctrl-/:toggle-preview'"
    ];
    # ALT-C
    changeDirWidgetOptions = ["--preview 'eza --tree --icons=auto --color=always {}'"];
    # tmux
    tmux.enableShellIntegration = true;
    tmux.shellIntegrationOptions = ["-p90%,80%"];
  };

  programs.granted = {
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
    config = {
      global.load_dotenv = true;
      whitelist.prefix = ["~/src/o11n"];
    };
  };

  programs.eza = {
    enable = true;
    # creates these aliases
    # ls = "eza";
    # ll = "eza -l";
    # la = "eza -a";
    # lt = "eza --tree";
    # lla = "eza -la";
  };

  programs.tmux = {
    enable = true;
    shortcut = "j";
    baseIndex = 1;
    # Stop tmux+escape printing nonsense
    # https://github.com/tmux-plugins/tmux-sensible/issues/61
    escapeTime = 1;
    mouse = true;
    keyMode = "vi";

    newSession = false;
    # Force tmux to use /tmp for sockets (WSL2 compat)
    secureSocket = false;
    terminal = "tmux-256color";
    clock24 = true;
    plugins = with pkgs.tmuxPlugins; [
      # https://github.com/dracula/tmux/blob/master/docs/CONFIG.md
      # TODO: Move to module and share with bluestreak
      # Weather uses: https://wttr.in/:help
      {
        plugin = dracula;
        extraConfig = ''
          set -g @dracula-plugins "time"
          set -g @dracula-show-timezone false
          set -g @dracula-show-location false
          set -g @dracula-fixed-location "Wappingers Falls, NY"
          set -g @dracula-show-battery false
          set -g @dracula-show-powerline true
          set -g @dracula-refresh-rate 10
          set -g @dracula-show-left-icon "#S"
          # for left
          set -g @dracula-show-left-sep 
          # for right symbol (can set any symbol you like as separator)
          set -g @dracula-show-right-sep 
          bind-key R source-file ~/.config/tmux/tmux.conf \; display "Reloaded!"
        '';
      }
      # # Then resurrect and continuum pair
      # {
      #   plugin = resurrect;
      #   extraConfig = ''
      #     set -g @resurrect-capture-pane-contents 'on'
      #   '';
      # }
      # {
      #   plugin = continuum;
      #   extraConfig = ''
      #     set -g @continuum-restore 'on'
      #     set -g @continuum-boot 'on'
      #     set -g @continuum-save-interval '15' # minutes
      #   '';
      # }
      sensible
      better-mouse-mode
      extrakto # prefix + tab
      fzf-tmux-url # prefix + u
      pain-control # sensible splits and movement
      tmux-thumbs # prefix + space
      # TODO: add TMUX_FZF_MENU= for custom menu using extraConfig, for assume and pass
      # https://github.com/sainnhe/tmux-fzf#user-menu
      tmux-fzf
      yank
    ];

    extraConfig = builtins.readFile ./includes/tmux.conf;
  };

  home.file.".gemrc".text = "gem: --no-ri --no-rdoc";
  home.file.".p10k.zsh".source = ./includes/p10k.zsh;

  home.file.".icons/default".source = "${pkgs.vanilla-dmz}/share/icons/Vanilla-DMZ";

  home.file.".xterm-24bit.terminfo" = {
    source = ./includes/xterm-24bit.terminfo;
    onChange = "tic -x -o ~/.terminfo ~/.xterm-24bit.terminfo";
  };

  home.shellAliases = rec {
    lls = "${pkgs.eza}/bin/eza --color=auto --group-directories-first --classify";
    lll = "${lls} --all --long --header --group";
    cdtemp = "cd `mktemp -df`";
    # git
    gst = "git status";
    gpo = "git push origin HEAD";
    gpu = "git pull --prune --tags --all";
    repo = "git browse >/dev/null";

    # SSH
    ssh = "TERM=xterm-256color ssh";

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
