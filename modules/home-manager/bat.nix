# ABOUTME: Reusable bat configuration module for home-manager
# ABOUTME: Provides bat with extras (batman, batgrep, batwatch) for enhanced file viewing
{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.webframp.bat;
in {
  options.webframp.bat = {
    enable = mkEnableOption "custom bat configuration";
  };

  config = mkIf cfg.enable {
    programs.bat = {
      enable = true;
      extraPackages = with pkgs.bat-extras; [batman batgrep batwatch];
    };

    # Alias man to batman for better man page viewing
    home.shellAliases.man = "batman";
  };
}
