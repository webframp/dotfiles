# inspired by:
# https://github.com/NixOS/nixpkgs/blob/nixos-22.11/pkgs/tools/security/vault/vault-bin.nix#L55
# with import <nixpkgs> {};
{ lib, stdenv, fetchurl }:
let
  pname = "granted";
  version = "0.14.0";
in stdenv.mkDerivation {
  inherit pname version;
  sourceRoot = ".";
  src = fetchurl {
    url =
      "https://releases.commonfate.io/${pname}/v${version}/${pname}_${version}_linux_x86_64.tar.gz";
    hash = "sha256-wHeh+BbeMtbJMEDZxOvihqcNpA4pR0s0Re0vhxcbq88=";
  };

  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    runHook preInstall
    install -D assume $out/bin/assume
    install -D assumego $out/bin/assumego
    install -D granted $out/bin/granted
    runHook postInstall
  '';

  doInstallCheck = true;
  installCheckPhase = ''
    runHook preInstallCheck
    $out/bin/granted -v
    runHook postInstallCheck
  '';

  dontPatchELF = true;

  # postInstall = ''
  #   # only if preference is to use non-wsl browser
  #   $out/bin/granted browser set -b firefox -p /mnt/c/Users/sme/scoop/shims/firefox.exe
  #   # only if preference is to use firefox as sso browser
  #   $out/bin/granted browser set-sso -b firefox
  # '';

  meta = with lib; {
    description = "A cli tool which simplifies access to cloud roles";
    homepage = "https://granted.dev";
    mainProgram = "granted";
  };
}
