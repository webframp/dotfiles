# Custom packages, defined similarly to nixpkgs
# Build using 'nix build .#example' or (legacy) 'nix-build -A example'
{pkgs ? import <nixpkgs> {}}: rec {
  aws-doctor = pkgs.callPackage ./aws-doctor {};
  iamlive = pkgs.callPackage ./iamlive {};
  # jwlm = pkgs.callPackage ./jwlm {};
}
