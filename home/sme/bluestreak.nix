# ABOUTME: Home-manager configuration for bluestreak (macOS Apple Silicon)
# ABOUTME: Primary workstation with full development environment
{
  outputs,
  lib,
  pkgs,
  ...
}: let
  gpgKey = "BE06ADB38C7F719D";
in {
  imports = [./shared/base.nix];

  # macOS-specific nix settings
  nix.settings.auto-optimise-store = false;

  nixpkgs = {
    config.allowUnfreePredicate = _: true;
    overlays = [outputs.overlays.additions];
  };

  home = {
    username = "sme";
    homeDirectory = "/Users/sme";
    stateVersion = "24.05";
    # Extra packages beyond base.nix (lists merge automatically)
    packages = with pkgs; [
      aws-doctor # custom package
      devcontainer
      emacs-lsp-booster
      mcp-k8s-go # https://github.com/strowk/mcp-k8s-go
    ];
  };

  # Bluestreak-specific webframp overrides
  webframp.git.credentialHelper = "!pass-git-helper $@";
  webframp.tmux.enableOrgCapture = true;

  webframp.zsh = {
    enableVterm = true;

    extraShellAliases = {
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
      default-key = gpgKey;
      no-tty = true;
      use-agent = true;
    };
  };

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 84000;
    maxCacheTtl = 84000;
    pinentry.package = pkgs.pinentry_mac;
  };

  # doom emacs setup is still manual
  programs.emacs = {
    enable = true;
    package = pkgs.emacs.override {withNativeCompilation = true;};
  };

  programs.keychain = {
    enable = true;
    extraFlags = ["--nogui" "--noask" "--quiet"];
    keys = ["id_ed25519" gpgKey];
  };

  services.home-manager.autoExpire = {
    enable = true;
    frequency = "weekly";
    timestamp = "-7 days";
  };
}
