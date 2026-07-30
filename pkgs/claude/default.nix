# ABOUTME: Claude Code - Anthropic's agentic coding CLI
# ABOUTME: Fetches prebuilt binaries directly from downloads.claude.ai, tracking
# ABOUTME: upstream releases instead of waiting on the nixpkgs packaging cadence.
# Update via 'make bump PKG=claude' (see scripts/update-claude.sh).
{
  lib,
  stdenvNoCC,
  fetchurl,
  installShellFiles,
  makeBinaryWrapper,
  autoPatchelfHook,
  alsa-lib,
  procps,
  ripgrep,
  bubblewrap,
  socat,
  versionCheckHook,
  writableTmpDirAsHomeHook,
  ...
}:
let
  stdenv = stdenvNoCC;
  baseUrl = "https://downloads.claude.ai/claude-code-releases";
  manifest = lib.importJSON ./manifest.json;
  platformKey = "${stdenv.hostPlatform.node.platform}-${stdenv.hostPlatform.node.arch}";
  platformManifestEntry = manifest.platforms.${platformKey};
in
  stdenv.mkDerivation (finalAttrs: {
    pname = "claude-code";
    inherit (manifest) version;

    src = fetchurl {
      url = "${baseUrl}/${finalAttrs.version}/${platformKey}/claude";
      sha256 = platformManifestEntry.checksum;
    };

    dontUnpack = true;
    dontBuild = true;
    __noChroot = stdenv.hostPlatform.isDarwin;
    # otherwise the bun runtime is executed instead of the binary
    dontStrip = true;

    nativeBuildInputs =
      [
        installShellFiles
        makeBinaryWrapper
      ]
      ++ lib.optionals stdenv.hostPlatform.isElf [autoPatchelfHook];

    strictDeps = true;

    installPhase = ''
      runHook preInstall

      installBin $src

      wrapProgram $out/bin/claude \
        --set DISABLE_AUTOUPDATER 1 \
        --set DISABLE_INSTALLATION_CHECKS 1 \
        --set USE_BUILTIN_RIPGREP 0 \
        ${lib.optionalString stdenv.hostPlatform.isLinux ''
        --prefix LD_LIBRARY_PATH : ${lib.makeLibraryPath [alsa-lib]} \
      ''}--prefix PATH : ${
        lib.makeBinPath (
          [
            procps
            ripgrep
          ]
          ++ lib.optionals stdenv.hostPlatform.isLinux [
            bubblewrap
            socat
          ]
        )
      }

      runHook postInstall
    '';

    doInstallCheck = true;
    nativeInstallCheckInputs = [
      writableTmpDirAsHomeHook
      versionCheckHook
    ];
    versionCheckKeepEnvironment = ["HOME"];
    versionCheckProgramArg = "--version";

    meta = {
      description = "Agentic coding tool that lives in your terminal, understands your codebase, and helps you code faster";
      homepage = "https://github.com/anthropics/claude-code";
      downloadPage = "https://claude.com/product/claude-code";
      changelog = "https://github.com/anthropics/claude-code/blob/v${finalAttrs.version}/CHANGELOG.md";
      license = lib.licenses.unfree;
      sourceProvenance = with lib.sourceTypes; [binaryNativeCode];
      platforms = [
        "aarch64-darwin"
        "x86_64-darwin"
        "aarch64-linux"
        "x86_64-linux"
      ];
      mainProgram = "claude";
    };
  })
