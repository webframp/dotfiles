{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  ...
}: {
  imports = [
    inputs.home-manager.darwinModules.home-manager
  ];

  nixpkgs = {
    hostPlatform = lib.mkDefault "aarch64-darwin";
    config = {
      allowUnfree = lib.mkDefault true;
      allowUnfreePredicate = lib.mkDefault true;
    };
  };

  nix = {
    settings = {
      # enable flakes + nix
      experimental-features = "nix-command flakes";
      auto-optimise-store = true;
      trusted-users = ["root" "sme"];

      # Binary cache setup
      # Run "cachix use <repo>" take values from ~/.config/nix/nix.conf
      # Then remove generated conf file
      substituters = ["https://nix-community.cachix.org" "https://cache.nixos.org"];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      ];
    };
  };

  # home.shellAliases = {
  #   yay = "sudo darwin-rebuild switch --flake .#bluestreak";
  # };

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    users.sme = import ../../home/sme/bluestreak.nix;
  };
}
