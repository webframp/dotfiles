{
  lib,
  stdenv,
  buildGoModule,
  fetchFromGitHub,
  ...
}:
buildGoModule rec {
  pname = "iamlive";
  version = "1.1.27";

  src = fetchFromGitHub {
    owner = "iann0036";
    repo = "iamlive";
    rev = "v${version}";
    hash = "sha256-xSftr7K3nRr8pgj0tzJjhH/SUnu4YTyMya+3vGK2qCY=";
  };

  vendorHash = null;

  meta = with lib; {
    description = "Generate an IAM policy from AWS calls using client-side monitoring (CSM)";
    homepage = "https://github.com/iann0036/iamlive";
    platforms = platforms.linux ++ platforms.darwin;
    license = licenses.asl20;
  };
}
