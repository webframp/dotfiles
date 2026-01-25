# ABOUTME: Shared tmux configuration module with dracula theme
# ABOUTME: Provides resurrect/continuum and common plugins across hosts
{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.webframp.tmux;
in {
  options.webframp.tmux = {
    enable = mkEnableOption "custom tmux configuration";

    enableOrgCapture = mkOption {
      type = types.bool;
      default = false;
      description = "Enable org-capture keybinding (macOS/Emacs)";
    };

    terminal = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = "Set terminal type (e.g., tmux-256color for NixOS/WSL)";
    };
  };

  config = mkIf cfg.enable {
    programs.tmux = {
      enable = true;
      shortcut = "j";
      baseIndex = 1;
      escapeTime = 1;
      mouse = true;
      keyMode = "vi";
      newSession = false;
      secureSocket = false; # WSL2 compat
      clock24 = true;
      sensibleOnTop = false;

      # Sensible defaults (replaces sensible plugin)
      historyLimit = 50000;
      focusEvents = true;
      aggressiveResize = true;

      terminal = mkIf (cfg.terminal != null) cfg.terminal;

      plugins = with pkgs.tmuxPlugins; [
        {
          plugin = dracula;
          extraConfig =
            ''
              set -g @dracula-plugins "time"
              set -g @dracula-show-timezone false
              set -g @dracula-show-location false
              set -g @dracula-show-battery false
              set -g @dracula-show-powerline true
              set -g @dracula-refresh-rate 10
              set -g @dracula-show-left-icon "#S"
              set -g @dracula-show-left-sep ""
              set -g @dracula-show-right-sep ""
              bind-key R source-file ~/.config/tmux/tmux.conf \; display "Reloaded!"
            ''
            + optionalString cfg.enableOrgCapture ''
              bind-key O run-shell -b "sh -c '~/.config/emacs/bin/org-capture' | grep -v '^nil$' || true"
            '';
        }
        {
          plugin = resurrect;
          extraConfig = ''
            set -g @resurrect-capture-pane-contents 'on'
          '';
        }
        {
          plugin = continuum;
          extraConfig = ''
            set -g @continuum-restore 'on'
            set -g @continuum-boot 'on'
            set -g @continuum-save-interval '15'
          '';
        }
        better-mouse-mode
        fzf-tmux-url
        pain-control
        tmux-thumbs
        tmux-fzf
        yank
      ];

      extraConfig = builtins.readFile ../../home/sme/global/includes/tmux.conf;
    };
  };
}
