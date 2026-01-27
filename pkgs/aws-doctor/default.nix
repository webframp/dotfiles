# ABOUTME: Nix package definition for aws-doctor CLI tool
# ABOUTME: AWS health check tool for cost anomalies and idle resource detection
{
  lib,
  buildGoModule,
  fetchFromGitHub,
  ...
}:
buildGoModule rec {
  pname = "aws-doctor";
  version = "1.3.0";

  src = fetchFromGitHub {
    owner = "elC0mpa";
    repo = "aws-doctor";
    rev = "v${version}";
    hash = "sha256-AnzJAoehOR7OUPfAWW0vlAhwDfQNNNq+vgB+Y7SPLqU=";
  };

  vendorHash = "sha256-SThgxOFwGmGutzdGIbHTv9AXBz9Rd3Kr1f07tdFHmpY=";

  ldflags = [
    "-s"
    "-w"
    "-X main.version=${version}"
    "-X main.commit=v${version}"
  ];

  meta = with lib; {
    description = "Terminal CLI for AWS health checks, cost anomalies, and idle resource detection";
    homepage = "https://github.com/elC0mpa/aws-doctor";
    platforms = platforms.linux ++ platforms.darwin;
    license = licenses.mit;
  };
}
