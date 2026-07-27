# ABOUTME: Swamp CLI - AI automation platform for developers
# ABOUTME: Fetches prebuilt binaries from GitHub releases
{
  lib,
  stdenv,
  fetchurl,
  makeWrapper,
  ...
}:
let
  version = "20260727.193526.0-sha.03dabbb5";

  sources = {
    x86_64-linux = {
      url = "https://github.com/swamp-club/swamp/releases/download/v${version}/swamp-linux-x86_64";
      hash = "sha256-3XIaGW18gQT58sCh9b4mF3lYLSKm3fcLtKd3LXueFqY=";
    };
    aarch64-darwin = {
      url = "https://github.com/swamp-club/swamp/releases/download/v${version}/swamp-darwin-aarch64";
      hash = "sha256-yL3bQI+J20sTsrteZaN1lrQcEHhXTmmAfE5wQjj/pK0=";
    };
  };

  src = fetchurl sources.${stdenv.system};
in
stdenv.mkDerivation {
  pname = "swamp";
  inherit version src;

  dontUnpack = true;
  dontStrip = true;
  dontPatchELF = true;

  nativeBuildInputs = [makeWrapper];

  installPhase = ''
    runHook preInstall
    install -Dm755 $src $out/libexec/swamp
    makeWrapper $out/libexec/swamp $out/bin/swamp \
      ${lib.optionalString stdenv.isLinux "--prefix LD_LIBRARY_PATH : ${stdenv.cc.cc.lib}/lib"}
    runHook postInstall
  '';

  meta = with lib; {
    description = "AI automation platform for developers with human review controls";
    homepage = "https://swamp.club";
    license = licenses.asl20;
    platforms = builtins.attrNames sources;
    mainProgram = "swamp";
  };
}
