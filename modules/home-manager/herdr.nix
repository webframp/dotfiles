# ABOUTME: Herdr terminal workspace manager package and config.toml
# ABOUTME: Generates config.toml from Nix and templates the URL opener per host
{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  cfg = config.webframp.herdr;
  toml = pkgs.formats.toml {};
in {
  options.webframp.herdr = {
    enable = lib.mkEnableOption "herdr terminal workspace manager";

    urlOpener = lib.mkOption {
      type = lib.types.str;
      default =
        if pkgs.stdenv.hostPlatform.isDarwin
        then "open"
        else "xdg-open";
      description = "Command the prefix+u URL popup passes selected URLs to (e.g. wslview for WSL)";
    };

    settings = lib.mkOption {
      inherit (toml) type;
      default = {};
      description = "herdr config.toml contents. Hosts can add to these; lists such as keys.command concatenate";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [inputs.herdr.packages.${pkgs.stdenv.hostPlatform.system}.default];

    # Port of modules/home-manager/tmux.nix, targeting herdr v0.9.3. Invalid
    # keys fall back to defaults with a startup warning, so check `prefix+?`
    # after changes to confirm what parsed.
    webframp.herdr.settings = {
      onboarding = false;

      theme.name = "dracula"; # built-in; replaces the dracula tmux plugin

      terminal.new_cwd = "follow";

      ui = {
        tab_bar_position = "bottom"; # tmux status line position
        prompt_new_tab_name = false; # tmux `c` doesn't prompt
        pane_gaps = false; # tmux-style shared dividers
        pane_outer_borders = false;
        show_agent_labels_on_pane_borders = true;
        window_title = "{terminal_title} / {tab}"; # set-titles-string "#T / #W"
        tab_bar_right = [
          {type = "zoom";}
          {
            type = "datetime";
            format = "%H:%M";
          } # clock24 + @dracula-plugins "time"
        ];
        toast.delivery = "terminal"; # outer-terminal notification; may replace the WezTerm bell->toast hook
      };

      keys = {
        prefix = "ctrl+j"; # programs.tmux.shortcut = "j"
        detach = "prefix+d"; # tmux default (herdr default is q)

        # Already identical to tmux/herdr defaults, listed for clarity:
        #   c new tab, x close pane, z zoom, [ copy mode, 1..9 tabs,
        #   h/j/k/l focus panes (pain-control), R reload config

        # pain-control splits: | and \ side-by-side, - and _ stacked
        split_vertical = ["prefix+|" "prefix+backslash" "prefix+v"];
        split_horizontal = ["prefix+minus" "prefix+_"];

        # pain-control H/J/K/L resize. Herdr puts swap on these by default, so move swap.
        resize_pane_left = "prefix+shift+h";
        resize_pane_down = "prefix+shift+j";
        resize_pane_up = "prefix+shift+k";
        resize_pane_right = "prefix+shift+l";
        swap_pane_left = "prefix+alt+h";
        swap_pane_down = "prefix+alt+j";
        swap_pane_up = "prefix+alt+k";
        swap_pane_right = "prefix+alt+l";

        # bind -r C-h / C-l previous/next window (no -r repeat equivalent in herdr)
        previous_tab = ["prefix+ctrl+h" "prefix+p"];
        next_tab = ["prefix+ctrl+l" "prefix+n"];

        # bind Tab last-window. Herdr's last_pane crosses tabs and workspaces.
        last_pane = "prefix+tab";
        cycle_pane_next = "prefix+space";
        cycle_pane_previous = "prefix+alt+space";

        # bind B / BTab tmux-jump-bell -> herdr's native agent attention model.
        # BTab jumps to the visible notification; B opens the picker (press `b` for blocked).
        open_notification_target = ["prefix+o" "prefix+shift+tab"];
        goto = ["prefix+g" "prefix+B"];

        command = [
          # fzf-tmux-url replacement: pull URLs from the active pane into fzf.
          {
            key = "prefix+u";
            type = "popup";
            description = "open URL from pane";
            width = "80%";
            height = "60%";
            command = ''"$HERDR_BIN_PATH" pane read "$HERDR_ACTIVE_PANE_ID" --source recent-unwrapped --lines 2000 | grep -oE 'https?://[^[:space:]"<>]+' | sort -u | fzf --tac | xargs -r ${cfg.urlOpener}'';
          }
          # Shelley on exe.dev VMs (webframp/shelley-herdr plugin): VM/conversation picker.
          # Needs the plugin registered once: `herdr plugin link <path to plugin>`.
          {
            key = "prefix+a";
            type = "plugin_action";
            command = "webframp.shelley.open";
            description = "Shelley on an exe.dev VM";
          }
        ];
      };
    };

    xdg.configFile."herdr/config.toml".source = toml.generate "herdr-config.toml" cfg.settings;
  };
}
