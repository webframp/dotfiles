{
  pkgs,
  inputs,
  ...
}: {
  imports = [./hardware-configuration.nix ./configuration.nix];

  # networking = {
  #   hostName = "galvatron-wsl";
  #   useDHCP = true;
  # };
  system.stateVersion = "23.05";
}
