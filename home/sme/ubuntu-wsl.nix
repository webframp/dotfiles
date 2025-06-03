# Pull in globals
{
  inputs,
  lib,
  pkgs,
  ...
}: {
  imports = [./global];

  home.shellAliases = {yay = "home-manager switch --flake .#sme@ubuntu";};

  programs.keychain = {
    enable = true;
    enableZshIntegration = true;
    keys = ["id_ed25519"];
  };
}
