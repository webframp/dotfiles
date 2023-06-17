{
  description = "Webframp nix config flake";

  inputs = {
    # Nixpkgs
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.05";
    # You can access packages and modules from different nixpkgs revs
    # at the same time. Here's an working example:
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    # Pure Nix flake utility functions.
    flake-utils.url = "github:numtide/flake-utils";

    # Home manager
    home-manager.url = "github:nix-community/home-manager/release-23.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    # NixOS-WSL: https://github.com/nix-community/NixOS-WSL/
    nixos-wsl.url = "github:nix-community/NixOS-WSL";

    # emacs overlay
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = { self, nixpkgs, home-manager, ... }@inputs:
    let
      inherit (self) outputs;
      forEachSystem = nixpkgs.lib.genAttrs [ "x86_64-linux" "x86_64-darwin" ];
      forEachPkgs = f: forEachSystem (sys: f nixpkgs.legacyPackages.${sys});

      mkNixos = modules:
        nixpkgs.lib.nixosSystem {
          inherit modules;
          specialArgs = { inherit inputs outputs; };
        };
      mkHome = modules: pkgs:
        home-manager.lib.homeManagerConfiguration {
          inherit modules pkgs;
          extraSpecialArgs = { inherit inputs outputs; };
        };
    in {
      # Custom packages
      # Acessible through 'nix build', 'nix shell', etc
      packages = forEachSystem (system:
        let pkgs = nixpkgs.legacyPackages.${system};
        in import ./pkgs { inherit pkgs; });

      # Devshell for bootstrapping, use 'nix develop'
      devShells = forEachPkgs (pkgs: import ./shell.nix { inherit pkgs; });
      formatter = forEachPkgs (pkgs: pkgs.nixpkgs-fmt);

      # Custom packages and modifications
      overlays = import ./overlays { inherit inputs; };
      # Stuff to upstream into nixpkgs
      nixosModules = import ./modules/nixos;
      # Stuff to upstream into home-manager
      homeManagerModules = import ./modules/home-manager;

      # NixOS entrypoint
      # Use:'nixos-rebuild --flake .#hostname'
      nixosConfigurations = { galvatron = mkNixos [ ./hosts/galvatron-wsl ]; };

      # Standalone home-manager entrypoints
      # Use: 'home-manager --flake .#username@your-hostname'
      homeConfigurations = {
        "sme@megatron" = mkHome [ ./home/sme/megatron.nix ]
          nixpkgs.legacyPackages."x86_64-darwin";
        "sme@ubuntu" = mkHome [ ./home/sme/ubuntu-wsl.nix ]
          nixpkgs.legacyPackages."x86_64-linux";
        "sme@generic" = mkHome [ ./home/sme/generic.nix ]
          nixpkgs.legacyPackages."x86_64-linux";
      };
    };
}
