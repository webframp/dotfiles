# ABOUTME: Nix package for go-jwlm CLI tool
# ABOUTME: Merges JW Library backups
{
  lib,
  buildGoModule,
  fetchFromGitHub,
  ...
}:
buildGoModule rec {
  pname = "go-jwlm";
  version = "0.5.7-beta";

  src = fetchFromGitHub {
    owner = "AndreasSko";
    repo = "go-jwlm";
    rev = version;
    hash = "sha256-2nwYBLHRDQwwvF5ULgaYcbnqSWW6QUa9Bt4SVLiGzSA=";
  };

  vendorHash = "sha256-jC5B3gLM0QA9KqqN0HGx9xlActcjyI3YAzI/M6POVjQ=";

  # Tests require network access (Test_DownloadCatalogRealLife)
  doCheck = false;

  meta = with lib; {
    description = "A command line tool to easily merge JW Library backups, written in Go.";
    homepage = "https://github.com/AndreasSko/go-jwlm";
    platforms = platforms.linux ++ platforms.darwin;
    license = licenses.mit;
  };
}
