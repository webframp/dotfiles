{
  lib,
  stdenv,
  buildGoModule,
  fetchFromGitHub,
  ...
}:
buildGoModule rec {
  pname = "go-jwlm";
  version = "0.5.6-beta";

  src = fetchFromGitHub {
    owner = "AndreasSko";
    repo = "go-jwlm";
    rev = "v${version}";
    hash = "";
  };

  vendorHash = null;

  meta = with lib; {
    description = "A command line tool to easily merge JW Library backups, written in Go.";
    homepage = "https://github.com/AndreasSko/go-jwlm";
    platforms = platforms.linux ++ platforms.darwin;
    license = licenses.mit;
  };
}
