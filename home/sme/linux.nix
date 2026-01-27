# ABOUTME: Linux-specific home-manager configuration shared across all Linux hosts
# ABOUTME: Imported by galvatron-wsl, ubuntu-wsl, and generic configurations
{pkgs, ...}: {
  gtk.enable = true;

  home.file.".gemrc".text = "gem: --no-ri --no-rdoc";

  home.file.".icons/default".source = "${pkgs.vanilla-dmz}/share/icons/Vanilla-DMZ";

  home.file.".xterm-24bit.terminfo" = {
    source = ./global/includes/xterm-24bit.terminfo;
    onChange = "tic -x -o ~/.terminfo ~/.xterm-24bit.terminfo";
  };

  # Linux-specific zsh settings
  webframp.zsh.extraEnvVars = ''
    export FORCE_NO_ALIAS=true
  '';

  # Ensure UTF-8
  home.language = {
    base = "en_US.UTF-8";
    ctype = "en_US.UTF-8";
    numeric = "en_US.UTF-8";
    time = "en_US.UTF-8";
    collate = "en_US.UTF-8";
    monetary = "en_US.UTF-8";
    messages = "en_US.UTF-8";
    name = "en_US.UTF-8";
  };
}
