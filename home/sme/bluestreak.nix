# ABOUTME: Home-manager configuration for bluestreak (macOS Apple Silicon)
# ABOUTME: Primary workstation with full development environment
{
  inputs,
  outputs,
  config,
  lib,
  pkgs,
  ...
}: {
  # You can import other home-manager modules here
  imports = with outputs.homeManagerModules; [
    # If you want to use home-manager modules from other flakes (such as nix-colors):
    # inputs.nix-colors.homeManagerModule

    # Shared configuration modules
    zsh
    bat
    delta
    direnv
    fzf
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
      # bash-my-aws
      # cdk8s-cli
      claude-code
      cmake
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
      glibtool
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
      # ollama
      (pass.withExtensions
        (ext: with ext; [pass-genphrase pass-otp pass-update]))
      pass-git-helper
      pinentry_mac
      pipx
      podman
      podman-compose
      pry
      ripgrep
      # spotify
      terraform
      terraform-docs
      tflint
      tflint-ruleset-aws
      tfsec
      urlscan
      wget
      unzip
      uv
      vale
      vault
      yt-dlp
      # zbar
      zip
      zstd
      zoxide

      # kubernetes
      kind
      kubectl
      kubernetes-helm
      kustomize
      go
      clang
      mcp-k8s-go # https://github.com/strowk/mcp-k8s-go

      # Nix related
      alejandra
      cachix
      direnv
      nix-index

      nox
      patchelf

      # emacs
      emacs-lsp-booster
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

  # Bluestreak-specific shell aliases (base aliases come from custom.zsh module)
  custom.zsh.extraShellAliases = {
    # Home-manager
    yay = "home-manager switch --flake .#sme@bluestreak";
    yayb = "brew update && brew upgrade && brew cleanup";
    news = "home-manager news --flake .";
    nixcleanup = "nix profile wipe-history --older-than 14d && nix-collect-garbage";

    # macOS-specific
    docker = "podman";

    # Doom Emacs
    doom = "~/.config/emacs/bin/doom";

    # Kubernetes
    k = "kubectl";
    kn = "kswitch ns";
    ks = "kswitch";
    kx = "kswitch";
  };

  # Shared module configuration
  custom.bat.enable = true;
  custom.delta.enable = true;
  custom.direnv = {
    enable = true;
    whitelist = ["~/src/o11n"];
  };
  custom.fzf.enable = true;

  # Zsh configuration via shared module
  # startup speed checking: for i in $(seq 1 5); do /run/current-system/sw/bin/time -p ~/.nix-profile/bin/zsh -i -c exit; done
  custom.zsh = {
    enable = true;
    enableVterm = true;

    extraEnvVars = ''
      export AWS_VAULT_BACKEND=pass
      export PODMAN_COMPOSE_WARNING_LOGS=false
      export CLAUDE_CODE_USE_BEDROCK=1
      # homebrew is not managed via nix, but a necessary evil on macOS
      [ -d /opt/homebrew/bin ] && export PATH="/opt/homebrew/bin:$PATH"
    '';

    extraZplugPlugins = [
      {
        # https://github.com/wfxr/forgit
        name = "webframp/zsh-forgit";
        tags = ["defer:0"];
      }
      {
        # https://github.com/hlissner/zsh-autopair
        name = "webframp/zsh-autopair";
        tags = ["defer:2"];
      }
      {
        name = "webframp/zsh-plugins";
        tags = ["defer:2" "use:rationalize-dot.plugin.zsh"];
      }
      {name = "webframp/zsh-you-should-use";}
      {
        name = "plugins/kubectl";
        tags = ["defer:2" "from:oh-my-zsh"];
      }
    ];
  };

  programs.git = {
    enable = true;
    signing.key = "BE06ADB38C7F719D";
    settings = {
      user.name = "Sean Escriva";
      user.email = "sean.escriva@gmail.com";
      alias = {
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

  # doom emacs setup is still manual
  programs.emacs = {
    enable = true;
    package = pkgs.emacs.override {withNativeCompilation = true;};
  };

  programs.eza.enable = true;
  programs.fastfetch.enable = true;
  programs.fd.enable = true;
  programs.zoxide.enable = true;

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
          set -g @continuum-boot-options 'fullscreen'
          set -g @continuum-save-interval '15' # minutes
        '';
      }
      better-mouse-mode
      # extrakto # prefix + tab
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
    keys = ["id_ed25519" "BE06ADB38C7F719D"];
  };

  programs.k9s.enable = true;
  programs.kubeswitch.enable = true;
  programs.granted.enable = true;
  programs.infat.enable = true;

  programs.jqp = {
    enable = true;
    settings = {
      theme = {
        name = "doom-one";
      };
    };
  };

  services.home-manager.autoExpire = {
    enable = true;
    frequency = "weekly";
    timestamp = "-7 days";
  };
}
