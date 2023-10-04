# Pull in globals
{
  inputs,
  lib,
  pkgs,
  ...
}: {
  imports = [./global];

  home.shellAliases = {yay = "home-manager switch --flake .#sme@ubuntu";};
}
