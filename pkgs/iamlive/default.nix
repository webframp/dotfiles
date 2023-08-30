{ lib, stdenv, buildGoModule, fetchFromGitHub, ... }:

buildGoModule rec {
  pname = "iamlive";
  version = "0.53.0";

  src = fetchFromGitHub {
    owner = "iann0036";
    repo = "iamlive";
    rev = "v${version}";
    hash = "sha256-0ly/egfzpbjyp4U2aFxW7cwaLkiV+sjwyjQHY62NHr4=";
  };

  vendorHash = null;

  meta = with lib; {
    description =
      "Generate an IAM policy from AWS calls using client-side monitoring (CSM)";
    homepage = "https://github.com/iann0036/iamlive";
    platforms = platforms.linux ++ platforms.darwin;
    license = licenses.asl20;
  };
}
