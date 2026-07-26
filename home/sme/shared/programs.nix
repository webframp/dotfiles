# ABOUTME: Shared program configurations enabled across all platforms
# ABOUTME: Imported by all host configurations for consistent tooling
{
  config,
  lib,
  pkgs,
  ...
}: let
  # Build the kubeswitch shell files at build time (same derivation home-manager uses)
  kubeswitchZshFiles = pkgs.runCommand "kubeswitch-${config.programs.kubeswitch.commandName}-shell-files-for-zsh"
    {
      nativeBuildInputs = [config.programs.kubeswitch.package];
    }
    ''
      mkdir -p $out/share
      switcher init "zsh" | sed "s/switch(/${config.programs.kubeswitch.commandName}(/" > "$out/share/${config.programs.kubeswitch.commandName}_init.zsh"
      switcher --cmd "${config.programs.kubeswitch.commandName}" completion "zsh" > "$out/share/${config.programs.kubeswitch.commandName}_completion.zsh"
    '';
in {
  programs.eza.enable = true;
  programs.fastfetch.enable = true;
  programs.fd.enable = true;
  programs.granted.enable = true;
  programs.k9s.enable = true;
  programs.kubeswitch = {
    enable = true;
    enableZshIntegration = false; # lazy-loaded below for startup speed
  };
  programs.zoxide.enable = true;

  # Lazy-load kubeswitch: define a stub that sources the real init on first call
  programs.zsh.initContent = lib.mkIf config.programs.kubeswitch.enable (lib.mkOrder 1100 ''
    kswitch() {
      unfunction kswitch
      source ${kubeswitchZshFiles}/share/${config.programs.kubeswitch.commandName}_init.zsh
      source ${kubeswitchZshFiles}/share/${config.programs.kubeswitch.commandName}_completion.zsh
      kswitch "$@"
    }
  '');

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
