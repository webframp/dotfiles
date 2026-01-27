# ABOUTME: Shared program configurations enabled across all platforms
# ABOUTME: Imported by all host configurations for consistent tooling
{
  lib,
  pkgs,
  ...
}: {
  programs.eza.enable = true;
  programs.fastfetch.enable = true;
  programs.fd.enable = true;
  programs.granted.enable = true;
  programs.k9s.enable = true;
  programs.kubeswitch.enable = true;
  programs.zoxide.enable = true;

  # Darwin-only programs
  programs.infat.enable = lib.mkIf pkgs.stdenv.isDarwin true;

  programs.jqp = {
    enable = true;
    settings = {
      theme = {
        name = "doom-one";
      };
    };
  };
}
