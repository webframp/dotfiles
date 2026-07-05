# ABOUTME: Reusable fzf configuration module for home-manager
# ABOUTME: Provides fzf with zsh integration, preview options, and tmux popup support
{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.webframp.fzf;
in {
  options.webframp.fzf = {
    enable = mkEnableOption "custom fzf configuration";
  };

  config = mkIf cfg.enable {
    programs.fzf = {
      enable = true;
      enableZshIntegration = true;
      # CTRL-T
      fileWidget.options = [
        "--preview 'bat -n --color=always {}'"
        " --bind 'ctrl-/:change-preview-window(down|hidden|)'"
      ];
      # CTRL-R
      historyWidget.options = [
        "--preview 'echo {}'"
        "--preview-window up:3:hidden:wrap"
        "--bind 'ctrl-/:toggle-preview'"
      ];
      # ALT-C
      changeDirWidget.options = ["--preview 'eza --tree --icons=auto --color=always {}'"];
      # tmux
      tmux.enableShellIntegration = true;
      tmux.shellIntegrationOptions = ["-p90%,80%"];
    };
  };
}
