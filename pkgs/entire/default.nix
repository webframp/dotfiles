# ABOUTME: Entire CLI fetched from official GitHub release binaries
# ABOUTME: Wraps the vendor curl|bash installer; no post-install step runs
{
  lib,
  stdenv,
  fetchzip,
  installShellFiles,
  ...
}: let
  version = "0.12.2";

  sources = {
    x86_64-linux = {
      url = "https://github.com/entireio/cli/releases/download/v${version}/entire_linux_amd64.tar.gz";
      hash = "sha256-7e6Wxs/9YKLsWga31mRW0KbZtD0fyZStPz2QCTipWKs=";
    };
    aarch64-darwin = {
      url = "https://github.com/entireio/cli/releases/download/v${version}/entire_darwin_arm64.tar.gz";
      hash = "sha256-nGKwxnfuqdDHeVGw/FNTRDxI9f1zfbJ+tMOXGuuIh6o=";
    };
  };

  # Binaries are statically linked Go executables, so no autoPatchelfHook is
  # needed. fetchzip unpacks the archive; stripRoot = false keeps the flat
  # layout (bare binaries plus completions/ at the root).
  src = fetchzip {
    inherit (sources.${stdenv.system}) url hash;
    stripRoot = false;
  };
in
  stdenv.mkDerivation {
    pname = "entire";
    inherit version src;

    dontUnpack = true;

    nativeBuildInputs = [installShellFiles];

    # The vendor installer runs `entire curl-bash-post-install` after dropping the
    # binary on disk — arbitrary vendor code as a side effect of "installing".
    # Packaging it as a derivation is exactly how we avoid that: we install the
    # binaries and nothing else. git-remote-entire ships in the same archive and
    # is required for `git clone entire://…` to resolve on PATH.
    installPhase = ''
      runHook preInstall
      install -Dm755 $src/entire $out/bin/entire
      install -Dm755 $src/git-remote-entire $out/bin/git-remote-entire
      runHook postInstall
    '';

    postInstall = ''
      installShellCompletion --cmd entire \
        --bash $src/completions/entire.bash \
        --zsh $src/completions/entire.zsh \
        --fish $src/completions/entire.fish
    '';

    meta = with lib; {
      description = "Entire CLI - command-line interface for the Entire service";
      homepage = "https://entire.com";
      license = licenses.unfree;
      platforms = builtins.attrNames sources;
      mainProgram = "entire";
    };
  }
