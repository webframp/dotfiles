# ABOUTME: Ghostty terminal configuration with the Swamp Club theme
# ABOUTME: Config only; the app itself is installed outside nix on macOS
{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.webframp.ghostty;

  # Swamp Club theme (MIT), vendored from upstream so it tracks the herdr and
  # WezTerm palettes. Ghostty resolves `theme = swamp-club` from ~/.config/ghostty/themes.
  swampClub = pkgs.fetchFromGitHub {
    owner = "sntxrr";
    repo = "ghostty-swamp-club";
    rev = "c275b3bfd6a39e0eabb7a826732f8997cc0ae6f2";
    hash = "sha256-jARVAJyb5iwZT6mX/R20Mjon9V1JB6h/DRg4M8y+lM8=";
  };
in {
  options.webframp.ghostty = {
    enable = mkEnableOption "custom ghostty configuration";
  };

  config = mkIf cfg.enable {
    programs.ghostty = {
      enable = true;
      # The macOS app is installed outside nix; nixpkgs does not build it for darwin.
      package = null;
      # Ghostty injects its own zsh integration into shells it launches. Sourcing
      # it from .zshrc as well would also run it in herdr panes, which inherit
      # GHOSTTY_RESOURCES_DIR, and leak its prompt marks into the p10k prompt.
      enableZshIntegration = false;
      settings = {
        theme = "swamp-club";
        background-opacity = 0.8;
        confirm-close-surface = false;
        font-family = "Hack Nerd Font Mono";
        font-size = 18;
        macos-titlebar-style = "hidden";
        maximize = true;
      };
    };

    xdg.configFile."ghostty/themes/swamp-club".source = "${swampClub}/themes/swamp-club";
  };
}
