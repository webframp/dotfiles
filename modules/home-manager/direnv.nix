# ABOUTME: Reusable direnv configuration module for home-manager
# ABOUTME: Provides direnv with nix-direnv integration and dotenv loading
{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.webframp.direnv;
in {
  options.webframp.direnv = {
    enable = mkEnableOption "custom direnv configuration";

    whitelist = mkOption {
      type = types.listOf types.str;
      default = [];
      description = "Directory prefixes to whitelist for automatic direnv loading";
    };
  };

  config = mkIf cfg.enable {
    programs.direnv = {
      enable = true;
      nix-direnv.enable = true;
      config = {
        global.load_dotenv = true;
        whitelist.prefix = cfg.whitelist;
      };
    };
  };
}
