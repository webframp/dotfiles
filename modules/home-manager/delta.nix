# ABOUTME: Reusable delta configuration module for home-manager
# ABOUTME: Provides delta git pager with syntax highlighting, complementing bat
{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.custom.delta;
in {
  options.custom.delta = {
    enable = mkEnableOption "custom delta configuration";
  };

  config = mkIf cfg.enable {
    programs.delta = {
      enable = true;
      enableGitIntegration = true;
      options = {
        navigate = true;
        features = "side-by-side line-numbers decorations";
        syntax-theme = "base16-256";
      };
    };
  };
}
