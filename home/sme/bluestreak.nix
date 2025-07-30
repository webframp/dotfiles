# This is your home-manager configuration file
# Use this to configure your home environment (it replaces ~/.config/nixpkgs/home.nix)
{
  inputs,
  config,
  lib,
  pkgs,
  ...
}: let
  # Custom tmux theme
  # Would like to move this to a separate file somehow
  tmux-tokyo-night = pkgs.tmuxPlugins.mkTmuxPlugin {
    pluginName = "tmux-tokyo-night";
    version = "1.5.2";
    src = pkgs.fetchFromGitHub {
      owner = "webframp";
      repo = "tmux-tokyo-night";
      rev = "d814e7c5aa4845edd1cb2c7e5c1f3524ca3ed82d";
      hash = "sha256-G5SV19811i0GBkXUDiQ5xerfkTxeQ9jdhM7k22XiQCg=";
    };
    rtpFilePath = "tmux-tokyo-night.tmux";
  };
in {
  # You can import other home-manager modules here
  imports = [
    # If you want to use home-manager modules from other flakes (such as nix-colors):
    # inputs.nix-colors.homeManagerModule

    # Lots of duplication in current config, but global isn't truly global or safe to use cross platform for now
    # ./global/default.nix
  ];

  nix = {
    package = lib.mkDefault pkgs.nix;
    settings = {
      auto-optimise-store = false;
      experimental-features = ["nix-command" "flakes"];
      warn-dirty = false;
    };
  };

  nixpkgs = {
    # You can add overlays here
    overlays = [
      # If you want to use overlays exported from other flakes:
      # neovim-nightly-overlay.overlays.default

      # Or define it inline, for example:
      # (final: prev: {
      #   hi = final.hello.overrideAttrs (oldAttrs: {
      #     patches = [ ./change-hello-to-hi.patch ];
      #   });
      # })
    ];
    config = {
      allowUnfree = true;
      # Workaround for https://github.com/nix-community/home-manager/issues/2942
      allowUnfreePredicate = _: true;
    };
  };

  home = {
    username = "sme";
    homeDirectory = "/Users/sme";
    stateVersion = "24.05";
    # should be macOS specific packages as needed
    # but right now is duplicating everything from global because global is busted cross platform
    packages = with pkgs;
    with nodePackages_latest;
    with tflint-plugins; [
      awscli2
      awslogs
      aws-vault
      aws-cdk
      # cdktf-cli
      # cdk8s-cli
      coder
      coreutils-prefixed
      delta
      devbox
      devcontainer
      dig
      exiftool
      fd
      ffmpeg
      file
      fzf
      git
      git-lfs
      git-extras
      gemini-cli
      # gitmux
      granted
      gnumake
      gnused
      htop
      hugo
      hyperfine
      inetutils
      imagemagick
      ispell
      jq
      yq-go
      tmux
      keychain
      mas
      mob
      nodejs_20
      ollama
      (pass.withExtensions
        (ext: with ext; [pass-genphrase pass-otp pass-update]))
      pass-git-helper
      pinentry_mac
      pipx
      podman
      podman-compose
      pry
      ripgrep
      spotify
      terraform
      terraform-docs
      tflint
      tflint-ruleset-aws
      tfsec
      urlscan
      wget
      unzip
      vale
      vault
      yt-dlp
      # zbar
      zip
      zoxide

      # kubernetes
      k9s
      kubectl
      kubectx
      kubernetes-helm
      kustomize
      go
      clang

      # Nix related
      alejandra
      cachix
      direnv
      nix-index

      nox
      patchelf

      # emacs
      vscode-langservers-extracted
      # bash-language-server
      cspell
      prettier
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
      pandoc
      shfmt
      shellcheck
      pngpaste
      graphviz
      stylelint
      js-beautify
      fontconfig

      # zplug seems to need
      perl
    ];
  };

  # Enable home-manager and git
  programs.home-manager.enable = true;

  home.shellAliases = rec {
    yay = "home-manager switch --flake .#sme@bluestreak";
    yayb = "brew update && brew upgrade && brew cleanup";
    news = "home-manager news --flake .";
    nixcleanup = "nix profile wipe-history --older-than 14d && nix-collect-garbage";
    lls = "${pkgs.eza}/bin/eza --color=auto --group-directories-first --classify";
    lll = "${lls} --all --long --header --group";
    cdtemp = "cd `mktemp -df`";
    docker = "podman";
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

    # doom
    doom = "~/.config/emacs/bin/doom";

    # kube
    k = "kubectl";
    kx = "kubectx";
    kn = "kubens";
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
      size = 100000;
      save = 100000;
    };

    # sessionVariables = {
    #   GRANTED_ALIAS_CONFIGURED = true;
    #   GRANTED_ENABLE_AUTO_REASSUME = false;
    #   GRANTED_QUIET = true;
    # };

    #  export TERM=xterm-24bit
    #  homebrew is not managed via nix, but a necessary evil on macOS
    envExtra = ''
      export AWS_VAULT_BACKEND=pass
      export ZSH_AUTOSUGGEST_USE_ASYNC=true
      export PATH="/opt/homebrew/bin:$PATH"
      export FORCE_NO_ALIAS=true
      export JSII_SILENCE_WARNING_UNTESTED_NODE_VERSION=true
      export PODMAN_COMPOSE_WARNING_LOGS=false
    '';

    initContent = builtins.readFile ./global/includes/zshrc;
    loginExtra = builtins.readFile ./global/includes/zlogin;
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

  programs.git = {
    enable = true;
    userName = "Sean Escriva";
    userEmail = "sean.escriva@gmail.com";
    signing.key = "BE06ADB38C7F719D";
    # delta a better pager: https://github.com/dandavison/delta
    delta.enable = true;
    delta.options = {
      navigate = true;
      fatures = "side-by-side line-numbers decorations";
      syntax-theme = "base16-256";
    };
    aliases = {
      "in" = "log ..@{upstream}";
      out = "log @{upstream}..";
      st = "status";
      co = "checkout";
      ci = "commit";
      br = "branch";
      r = "reset";
      rh = "reset --hard";
      rh1 = "reset --hard HEAD~1";
      rh2 = "reset --hard HEAD~2";
      stage = "add -p";
      ls = "log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --date=relative";
      ll = "log --pretty=format:'%Cred%h%C(yellow)%d%Creset -%Creset %s %Cgreen(%cr) %C(bold blue)<%cn>%Creset' --decorate --numstat";
      dlc = "diff --cached HEAD^";
      filelog = "log -u";
      fl = "log -u";
      serve = "daemon --reuseaddr --base-path=. --export-all --verbose";
      pos = "push -o ci.skip";
      pmr = "push origin HEAD --force-with-lease -o merge_request.remove_source_branch -o merge_request.create";
    };
    # Remaining options not specifically available through home-manager module
    extraConfig = {
      branch = {
        autosetuprebase = "always";
        sort = "committerdate";
      };
      color = {ui = true;};
      column = {ui = "auto";};
      core = {
        excludesfile = "~/.gitignore";
        attributesfile = "~/.gitattributes";
      };
      credential = {
        helper = "!pass-git-helper $@";
        useHttpPath = true;
      };
      diff = {
        algorithm = "histogram";
        colorMoved = "plain";
        mnemonicprefix = true;
        renames = true;
      };
      fetch = {
        all = true;
        prune = true;
        prunetags = true;
      };
      github = {user = "webframp";};
      gitlab = {user = "webframp";};
      help = {autocorrect = 1;};
      init = {defaultBranch = "main";};
      merge = {conflictStyle = "zdiff3";};
      protocol = {version = 2;};
      pull = {rebase = true;};
      push = {
        autosetupremote = true;
        default = "simple";
        followtags = true;
      };
      rebase = {
        autosquash = true;
        autostash = true;
        updaterefs = true;
      };
      rerere = {
        enabled = true;
        autoupdate = true;
      };
      tag = {sort = "version:refname";};
    };
    # multiple settings to allow emacs forge to work:
    # [gitlab "git.bethelservice.org/api/v4"]
    #   user = sescriva
    # includeIf directives
  };

  # programs.git-cliff.enable = true;

  programs.gpg = {
    enable = true;
    settings = {
      default-key = "BE06ADB38C7F719D"; # TODO don't love hardcoding this value twice
      no-tty = true;
      use-agent = true;
    };
    # TODO .gnupg/gpg-agent.conf is not yet managed via home manager
    # pinentry-program ~/.nix-profile/bin/pinentry-mac
    # default-cache-ttl 84000
    # max-cache-ttl 84000
  };

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
    config = {
      global.load_dotenv = true;
      whitelist.prefix = ["~/src/o11n"];
    };
  };

  # doom emacs setup is still manual
  programs.emacs = {
    enable = true;
    package = pkgs.emacs.override {withNativeCompilation = true;};
  };

  programs.eza.enable = true;
  programs.fastfetch.enable = true;
  programs.fd.enable = true;
  programs.zoxide.enable = true;
  programs.carapace.enable = true;

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
    sensibleOnTop = false;
    plugins = with pkgs.tmuxPlugins; [
      # First plugins that adjust the right status bar
      {
        plugin = dracula;
        extraConfig = ''
          set -g @dracula-plugins "weather"
          set -g @dracula-show-location false
          set -g @dracula-show-battery false
          set -g @dracula-show-powerline true
          set -g @dracula-refresh-rate 10
          set -g @dracula-show-left-icon "#S"
          # for left
          set -g @dracula-show-left-sep 
          # for right symbol (can set any symbol you like as separator)
          set -g @dracula-show-right-sep 
          bind-key R source-file ~/.config/tmux/tmux.conf \; display "Reloaded!"
          # quick bind for org-capture
          bind-key O run-shell -b "sh -c '~/.config/emacs/bin/org-capture' | grep -v '^nil$' || true"
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
          set -g @continuum-boot-options 'alacritty,fullscreen'
          set -g @continuum-save-interval '15' # minutes
        '';
      }
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

    extraConfig = builtins.readFile ./global/includes/tmux.conf;
  };

  programs.keychain = {
    enable = true;
    enableZshIntegration = true;
    keys = ["id_ed25519" "BE06ADB38C7F719D"];
  };

  programs.kubeswitch.enable = true;

  # TODO
  # programs.jqp
  # programs.zsh.initContent

  # Make sure VSCode is available and configured I guess?
  programs.vscode.profiles.default = {
    enable = true;
    extensions = with pkgs.vscode-extensions; [
      davidanson.vscode-markdownlint
      editorconfig.editorconfig
      esbenp.prettier-vscode
      continue.continue
      golang.go
      hashicorp.hcl
      hashicorp.terraform
      ms-azuretools.vscode-docker
      oderwat.indent-rainbow
      redhat.vscode-yaml
      # ms-dotnettools.dotnet-interactive-vscode
      ms-toolsai.jupyter
      ms-dotnettools.csharp
    ];
    userSettings = {
      "telemetry.telemetryLevel" = "off";
      "editor.renderWhitespace" = "all";
      "editor.defaultFormatter" = "esbenp.prettier-vscode";
    };
  };

  services.home-manager.autoExpire = {
    enable = true;
    frequency = "weekly";
    timestamp = "-7 days";
  };
}
