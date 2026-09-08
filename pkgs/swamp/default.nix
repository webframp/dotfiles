# ABOUTME: Swamp CLI - AI automation platform for developers
# ABOUTME: Fetches prebuilt binary tarballs from artifacts.swamp-club.com
{
  lib,
  stdenv,
  fetchurl,
  makeWrapper,
  ...
}:
let
  version = "20260908.175912.0-sha.14d401ea";

  sources = {
    x86_64-linux = {
      url = "https://artifacts.swamp-club.com/swamp/${version}/binary/linux/x86_64/swamp-${version}-binary-linux-x86_64.tar.gz";
      hash = "sha256-H7u/XuTb/nee93741WOdLi7KM70s5eesp1aTZy+GpBg=";
    };
    aarch64-darwin = {
      url = "https://artifacts.swamp-club.com/swamp/${version}/binary/darwin/aarch64/swamp-${version}-binary-darwin-aarch64.tar.gz";
      hash = "sha256-25pioZ0m3/5g7idRgYFGCCwoo0jRgRw+8jF7rp84D8A=";
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

  # The tarball's sole entry is the bare binary (no wrapping directory),
  # which the standard unpackPhase can't handle (it expects $sourceRoot to
  # be a directory), so extract it manually.
  installPhase = ''
    runHook preInstall
    tar -xzf $src -O > swamp
    install -Dm755 swamp $out/libexec/swamp
    makeWrapper $out/libexec/swamp $out/bin/swamp \
      ${lib.optionalString stdenv.hostPlatform.isLinux "--prefix LD_LIBRARY_PATH : ${stdenv.cc.cc.lib}/lib"}
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
