# ABOUTME: Reusable bat configuration module for home-manager
# ABOUTME: Provides bat with extras (batman, batgrep, batwatch) for enhanced file viewing
{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.custom.bat;
in {
  options.custom.bat = {
    enable = mkEnableOption "custom bat configuration";
  };

  config = mkIf cfg.enable {
    programs.bat = {
      enable = true;
      extraPackages = with pkgs.bat-extras; [batman batgrep batwatch];
    };
  };
}
