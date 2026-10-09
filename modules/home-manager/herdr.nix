# ABOUTME: Herdr terminal workspace manager package and config.toml
# ABOUTME: Installs herdr from its flake input and templates the URL opener per host
{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  cfg = config.webframp.herdr;
in {
  options.webframp.herdr = {
    enable = lib.mkEnableOption "herdr terminal workspace manager";

    urlOpener = lib.mkOption {
      type = lib.types.str;
      default =
        if pkgs.stdenv.hostPlatform.isDarwin
        then "open"
        else "xdg-open";
      description = "Command the prefix+u URL popup passes selected URLs to (e.g. wslview for WSL)";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [inputs.herdr.packages.${pkgs.stdenv.hostPlatform.system}.default];

    xdg.configFile."herdr/config.toml".text =
      builtins.replaceStrings ["@opener@"] [cfg.urlOpener]
      (builtins.readFile ../../home/sme/shared/includes/herdr.toml);
  };
}
