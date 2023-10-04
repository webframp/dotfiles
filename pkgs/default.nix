# Custom packages, defined similarly to nixpkgs
# Build using 'nix build .#example' or (legacy) 'nix-build -A example'
{pkgs ? import <nixpkgs> {}}: rec {
  iamlive = pkgs.callPackage ./iamlive {};
}
