# ABOUTME: Kiro CLI - AI-powered development assistant from AWS
# ABOUTME: Fetches standalone prebuilt archives from desktop-release.q.us-east-1.amazonaws.com
{
  lib,
  stdenv,
  fetchurl,
  autoPatchelfHook,
  ...
}: let
  version = "3.1.0";

  sources = {
    x86_64-linux = {
      url = "https://desktop-release.q.us-east-1.amazonaws.com/${version}/kirocli-x86_64-linux.tar.gz";
      hash = "sha256-AGg1svT0M/VbsCYBzTBTzLEL09QIR4cZwz0yDxjka6U=";
    };
    aarch64-darwin = {
      url = "https://desktop-release.q.us-east-1.amazonaws.com/${version}/kirocli-universal-darwin.tar.gz";
      hash = "sha256-yQemqrk+BASnXbZnQSrAmgEWPu/E8dc059IhpuA4cPA=";
    };
  };

  src = fetchurl (sources.${stdenv.hostPlatform.system} or (throw "Unsupported system: ${stdenv.hostPlatform.system}"));
in
  stdenv.mkDerivation {
    pname = "kiro-cli";
    inherit version src;

    sourceRoot = "kirocli";

    nativeBuildInputs = lib.optionals stdenv.hostPlatform.isLinux [autoPatchelfHook];

    buildInputs = lib.optionals stdenv.hostPlatform.isLinux [stdenv.cc.cc.lib];

    dontStrip = true;

    installPhase = ''
      runHook preInstall
      install -Dm755 bin/kiro-cli $out/bin/kiro-cli
      runHook postInstall
    '';

    meta = with lib; {
      description = "Kiro CLI - AI-powered development assistant";
      homepage = "https://kiro.dev";
      license = licenses.unfree;
      platforms = builtins.attrNames sources;
      mainProgram = "kiro-cli";
    };
  }
