{
  description = "Webframp nix config flake";

  inputs = {
    # Nixpkgs
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    # You can access packages and modules from different nixpkgs revs
    # at the same time. Here's an working example:
    # nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    # Pure Nix flake utility functions.
    flake-utils.url = "github:numtide/flake-utils";

    # Home manager
    home-manager.url = "github:nix-community/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    # NixOS-WSL: https://github.com/nix-community/NixOS-WSL/
    nixos-wsl.url = "github:nix-community/NixOS-WSL";

    # nix-darwin
    nix-darwin.url = "github:LnL7/nix-darwin/master";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";

    # emacs overlay
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    self,
    nixpkgs,
    home-manager,
    nix-darwin,
    ...
  } @ inputs: let
    inherit (self) outputs;
    forEachSystem = nixpkgs.lib.genAttrs ["x86_64-linux" "x86_64-darwin" "aarch64-darwin"];
    forEachPkgs = f: forEachSystem (sys: f nixpkgs.legacyPackages.${sys});

    mkNixos = modules:
      nixpkgs.lib.nixosSystem {
        inherit modules;
        specialArgs = {inherit inputs outputs;};
      };
    mkHome = modules: pkgs:
      home-manager.lib.homeManagerConfiguration {
        inherit modules pkgs;
        extraSpecialArgs = {inherit inputs outputs;};
      };
  in {
    # Custom packages
    # Acessible through 'nix build', 'nix shell', etc
    packages = forEachSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};
    in
      import ./pkgs {inherit pkgs;});

    # Devshell for bootstrapping, use 'nix develop'
    devShells = forEachPkgs (pkgs: import ./shell.nix {inherit pkgs;});
    formatter = forEachPkgs (pkgs: pkgs.alejandra);

    # Custom packages and modifications
    overlays = import ./overlays {inherit inputs;};
    # Stuff to upstream into nixpkgs
    nixosModules = import ./modules/nixos;
    # Stuff to upstream into home-manager
    # homeManagerModules = import ./modules/home-manager;

    # NixOS entrypoint
    # Use:'nixos-rebuild --flake .#hostname'
    nixosConfigurations = {galvatron = mkNixos [./hosts/galvatron-wsl];};

    # Standalone home-manager entrypoints
    # Use: 'home-manager --flake .#username@your-hostname'
    homeConfigurations = {
      "sme@bluestreak" =
        mkHome [./home/sme/bluestreak.nix]
        nixpkgs.legacyPackages."aarch64-darwin";
      "sme@ubuntu" =
        mkHome [./home/sme/ubuntu-wsl.nix]
        nixpkgs.legacyPackages."x86_64-linux";
      "sme@generic" =
        mkHome [./home/sme/generic.nix]
        nixpkgs.legacyPackages."x86_64-linux";
    };
  };
}
