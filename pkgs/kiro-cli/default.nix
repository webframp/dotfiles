# ABOUTME: Kiro CLI - AI-powered development assistant from AWS
# ABOUTME: Fetches prebuilt binaries from prod.download.cli.kiro.dev
{
  lib,
  stdenv,
  fetchurl,
  autoPatchelfHook,
  glibc,
  gcc,
  undmg,
  ...
}:
let
  version = "2.33.1";

  sources = {
    x86_64-linux = {
      url = "https://prod.download.cli.kiro.dev/stable/${version}/kirocli-x86_64-linux.tar.gz";
      hash = "sha256-WZMVir6xowhqWR1cfdd+PbPIiaPA2LQUGLxqGveUAPs=";
    };
    aarch64-darwin = {
      url = "https://prod.download.cli.kiro.dev/stable/${version}/Kiro%20CLI.dmg";
      hash = "sha256-9US0+d5RpFfh7QhJ/98r57a3CvSRMwAGsBy3H6IRH0Q=";
    };
  };

  src = fetchurl (sources.${stdenv.system} // lib.optionalAttrs stdenv.hostPlatform.isDarwin {
    name = "kiro-cli-${version}.dmg";
  });
in
stdenv.mkDerivation {
  pname = "kiro-cli";
  inherit version src;

  sourceRoot = if stdenv.hostPlatform.isLinux then "kirocli" else ".";

  nativeBuildInputs =
    lib.optionals stdenv.hostPlatform.isLinux [autoPatchelfHook]
    ++ lib.optionals stdenv.hostPlatform.isDarwin [undmg];

  buildInputs = lib.optionals stdenv.hostPlatform.isLinux [
    glibc
    gcc.cc.lib
  ];

  dontStrip = true;

  installPhase = ''
    runHook preInstall
  '' + lib.optionalString stdenv.hostPlatform.isLinux ''
    install -Dm755 bin/kiro-cli $out/bin/kiro-cli
    install -Dm755 bin/kiro-cli-chat $out/bin/kiro-cli-chat
    install -Dm755 bin/kiro-cli-term $out/bin/kiro-cli-term
  '' + lib.optionalString stdenv.hostPlatform.isDarwin ''
    install -Dm755 "Kiro CLI.app/Contents/MacOS/kiro-cli" $out/bin/kiro-cli
    install -Dm755 "Kiro CLI.app/Contents/MacOS/kiro-cli-chat" $out/bin/kiro-cli-chat
    install -Dm755 "Kiro CLI.app/Contents/MacOS/kiro-cli-term" $out/bin/kiro-cli-term
  '' + ''
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
