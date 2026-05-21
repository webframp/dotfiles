# ABOUTME: Coder CLI fetched from official release binaries
# ABOUTME: Pinned to match server version, avoiding nixpkgs lag
{
  lib,
  stdenv,
  fetchzip,
  installShellFiles,
  ...
}:
let
  version = "2.33.3";

  sources = {
    x86_64-linux = {
      url = "https://github.com/coder/coder/releases/download/v${version}/coder_${version}_linux_amd64.tar.gz";
      hash = "sha256-oQhPKCR154vtCPOBujsP0QNLta7hmmTsYscDTu+X55I=";
      extension = "tar.gz";
    };
    aarch64-darwin = {
      url = "https://github.com/coder/coder/releases/download/v${version}/coder_${version}_darwin_arm64.zip";
      hash = "sha256-l4KJY9FnRzCo1xKDqyUMGIejKhx7RQN08USWqgjol1U=";
      extension = "zip";
    };
  };

  src = fetchzip {
    inherit (sources.${stdenv.system}) url hash extension;
    stripRoot = false;
  };
in
stdenv.mkDerivation {
  pname = "coder";
  inherit version src;

  dontUnpack = true;

  nativeBuildInputs = [installShellFiles];

  installPhase = ''
    runHook preInstall
    install -Dm755 $src/coder $out/bin/coder
    runHook postInstall
  '';

  postInstall = ''
    installShellCompletion --cmd coder \
      --bash <($out/bin/coder completion bash) \
      --zsh <($out/bin/coder completion zsh) \
      --fish <($out/bin/coder completion fish)
  '';

  meta = with lib; {
    description = "Coder CLI - provision and manage self-hosted development environments";
    homepage = "https://coder.com";
    license = licenses.agpl3Only;
    platforms = builtins.attrNames sources;
    mainProgram = "coder";
  };
}
