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
      experimental-features = ["nix-command" "flakes" "repl-flake"];
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
      awscli2
      awslogs
      aws-vault
      aws-cdk
      cdktf-cli
      cdk8s-cli
      coreutils
      csvkit
      delta
      devbox
      dig
      fd
      file
      fzf
      git
      git-lfs
      git-extras
      # gitmux
      gnumake
      gnupg
      htop
      hugo
      inetutils
      ispell
      jq
      yq-go
      keychain
      mob
      nodejs_20
      nomad
      pry
      rain
      ripgrep
      terraform
      terraform-docs
      tflint
      tflint-ruleset-aws
      tfsec
      urlscan
      # Instead of: sudo ln -s /mnt/c/WINDOWS/system32/clip.exe /usr/bin/wl-copy
      # wl-clipboard
      wget
      unzip
      vanilla-dmz
      vault
      yt-dlp
      zbar
      zip
      zoxide

      # Nix related
      alejandra
      cachix
      direnv
      nix-index

      nox
      patchelf

      # emacs
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
      export TERM=xterm-24bit
      export ZSH_AUTOSUGGEST_USE_ASYNC=true;
    '';
    # alias assume="source ${pkgs.granted}/bin/.assume-wrapped"

    initExtra = builtins.readFile ./includes/zshrc;
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
    clock24 = true;
    plugins = with pkgs.tmuxPlugins; [
      # First plugins that adjust the right status bar
      {
        plugin = tmux-tokyo-night;
        extraConfig = ''
          set -g @theme_plugin_datetime_format '%b %d %H:%M'
          set -g @theme_left_separator ''
          set -g @theme_right_separator ''
        '';
      }
      # Then resurrect and continuum pair
      {
        plugin = resurrect;
        extraConfig = ''
          set -g @resurrect-capture-pane-contents 'on'
        '';
      }
      {
        plugin = continuum;
        extraConfig = ''
          set -g @continuum-restore 'on'
          set -g @continuum-boot 'on'
          set -g @continuum-save-interval '15' # minutes
        '';
      }
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
