# Custom packages, defined similarly to nixpkgs
# Build using 'nix build .#example' or (legacy) 'nix-build -A example'
{pkgs ? import <nixpkgs> {}}: rec {
  aws-doctor = pkgs.callPackage ./aws-doctor {};
  go-jwlm = pkgs.callPackage ./go-jwlm {};
  iamlive = pkgs.callPackage ./iamlive {};
}
