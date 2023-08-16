{ lib, stdenv, buildGoModule, fetchFromGitHub, ... }:

buildGoModule rec {
  pname = "risor";
  version = "0.13.0";

  src = fetchFromGitHub {
    owner = "risor-io";
    repo = "risor";
    rev = "v${version}";
    hash = "sha256-lalT9kwq0+y3xaYAcl/DqcwGXE27lNuL1DCr1wEE8ZE=";
  };

  vendorHash = "sha256-diAbQwnlhMm43ZlLKq3llMl9mO3sIkc80aCI5UDn7F4=";

  meta = with lib; {
    description = "Fast and flexible scripting for Go developers and DevOps.";
    homepage = "https://risor.io";
    platforms = platforms.linux ++ platforms.darwin;
    license = licenses.asl20;
  };
}
