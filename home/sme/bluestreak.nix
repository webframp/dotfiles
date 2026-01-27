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
  imports =
    [
      ./programs.nix
    ]
    ++ (with outputs.homeManagerModules; [
      # Shared configuration modules
      zsh
      bat
      delta
      direnv
      fzf
      git
      tmux
    ]);

  nix = {
    package = lib.mkDefault pkgs.nix;
    settings = {
      auto-optimise-store = false;
      experimental-features = ["nix-command" "flakes"];
      warn-dirty = false;
    };
  };

  nixpkgs = {
    overlays = [
      outputs.overlays.additions
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
    packages =
      import ./packages.nix {inherit lib pkgs;}
      ++ (with pkgs; [
        # bluestreak-specific extras
        aws-doctor # custom package
        devcontainer
        emacs-lsp-booster
        mcp-k8s-go # https://github.com/strowk/mcp-k8s-go
      ]);
  };

  # Enable home-manager and git
  programs.home-manager.enable = true;

  # Bluestreak-specific shell aliases (base aliases come from webframp.zsh module)
  webframp.zsh.extraShellAliases = {
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
  webframp.bat.enable = true;
  webframp.delta.enable = true;
  webframp.direnv = {
    enable = true;
    whitelist = ["~/src/o11n"];
  };
  webframp.fzf.enable = true;
  webframp.git = {
    enable = true;
    credentialHelper = "!pass-git-helper $@";
  };
  webframp.tmux = {
    enable = true;
    enableOrgCapture = true;
  };

  # Zsh configuration via shared module
  # startup speed checking: for i in $(seq 1 5); do /run/current-system/sw/bin/time -p ~/.nix-profile/bin/zsh -i -c exit; done
  webframp.zsh = {
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

  programs.keychain = {
    enable = true;
    keys = ["id_ed25519" "BE06ADB38C7F719D"];
  };

  services.home-manager.autoExpire = {
    enable = true;
    frequency = "weekly";
    timestamp = "-7 days";
  };
}
