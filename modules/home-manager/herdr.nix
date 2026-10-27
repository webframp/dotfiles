# ABOUTME: Herdr terminal workspace manager package and config.toml
# ABOUTME: Generates config.toml from Nix, including fzf project and agent-worktree popups
{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  cfg = config.webframp.herdr;
  toml = pkgs.formats.toml {};

  herdr-launch = pkgs.writeShellApplication {
    name = "herdr-launch";
    runtimeInputs = with pkgs; [coreutils fd flock fzf git jq zoxide];
    text = builtins.readFile ../../home/sme/shared/includes/herdr-launch.sh;
  };

  herdr-record = pkgs.writeShellApplication {
    name = "herdr-record";
    runtimeInputs = with pkgs; [asciinema coreutils gnused jq];
    text = builtins.readFile ../../home/sme/shared/includes/herdr-record.sh;
  };

  # Popup commands run through a shell, so export the module's settings inline.
  launch = args:
    lib.concatStringsSep " " [
      "HERDR_PROJECT_ROOTS=${lib.escapeShellArg (lib.concatStringsSep ":" cfg.projectRoots)}"
      "HERDR_WORKTREES_DIR=${lib.escapeShellArg cfg.worktreesDirectory}"
      (lib.getExe herdr-launch)
      args
    ];

  # The recording popup needs its own env: where casts go and which font the
  # recorded shell's companion renderer expects.
  record = args:
    lib.concatStringsSep " " [
      "HERDR_RECORDINGS_DIR=${lib.escapeShellArg cfg.recordingsDirectory}"
      "HERDR_ASCIINEMA_BIN=${lib.escapeShellArg (lib.getExe pkgs.asciinema)}"
      (lib.getExe herdr-record)
      args
    ];
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

    copyCommand = lib.mkOption {
      type = lib.types.str;
      default =
        if pkgs.stdenv.hostPlatform.isDarwin
        then "pbcopy"
        else "xclip -i -selection clipboard";
      description = "Command that writes stdin to the system clipboard, for the prefix+Y yank binding";
    };

    pasteCommand = lib.mkOption {
      type = lib.types.str;
      default =
        if pkgs.stdenv.hostPlatform.isDarwin
        then "pbpaste"
        else "xclip -o -selection clipboard";
      description = "Command that prints the system clipboard for the prefix+] paste binding";
    };

    projectRoots = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = ["~/src"];
      description = "Directories searched for git repos by the prefix+f project picker, after zoxide's history";
    };

    agentKinds = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = ["claude" "codex" "kiro" "pi"];
      description = "herdr agent kinds offered by prefix+shift+a, first is preselected";
    };

    agentArgs = lib.mkOption {
      type = lib.types.attrsOf (lib.types.listOf lib.types.str);
      default = {
        claude = ["--dangerously-skip-permissions"];
        codex = ["--dangerously-bypass-approvals-and-sandbox"];
        kiro = ["--v3"];
        pi = ["--approve"]; # pi never gates tools; this skips the project-local trust prompt
      };
      description = "Default CLI args per agent kind, prefilled in the prefix+shift+a popup's editable args prompt";
    };

    autoRenameTabs = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Name tabs after the command running in their zsh panes, and \"zsh\" at an idle prompt. Manually named tabs are left alone";
    };

    worktreesDirectory = lib.mkOption {
      type = lib.types.str;
      default = "~/.herdr/worktrees";
      description = "Root for herdr-created git worktrees (<dir>/<repo>/<branch-slug>)";
    };

    recordingsDirectory = lib.mkOption {
      type = lib.types.str;
      default = "~/recordings";
      description = "Directory the prefix+shift+s recording binding writes asciinema .cast files into. The @webframp/screencast swamp behavior renders them to MP4";
    };

    recordingFontFamily = lib.mkOption {
      type = lib.types.str;
      default = "JetBrainsMono Nerd Font Mono";
      description = "Monospace font the screencast behavior passes to agg when rendering. Must match an installed font; agg fails otherwise";
    };

    settings = lib.mkOption {
      inherit (toml) type;
      default = {};
      description = "herdr config.toml contents. Hosts can add to these; lists such as keys.command concatenate";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      inputs.herdr.packages.${pkgs.stdenv.hostPlatform.system}.default
      # Screencast pipeline: record sessions, render casts to MP4 for YouTube.
      pkgs.asciinema # terminal session recorder (prefix+shift+s binding)
      pkgs.asciinema-agg # `agg`: cast -> animated GIF
      pkgs.ffmpeg # GIF -> H.264 MP4
    ];

    # Port of modules/home-manager/tmux.nix, targeting herdr v0.9.3. Invalid
    # keys fall back to defaults with a startup warning, so check `prefix+?`
    # after changes to confirm what parsed.
    webframp.herdr.settings = {
      onboarding = false;

      theme.name = "dracula"; # built-in; replaces the dracula tmux plugin

      terminal.new_cwd = "follow";

      worktrees.directory = cfg.worktreesDirectory;

      ui = {
        tab_bar_position = "bottom"; # tmux status line position
        prompt_new_tab_name = false; # tmux `c` doesn't prompt
        pane_gaps = false; # tmux-style shared dividers
        pane_outer_borders = false;
        host_cursor = "drawn"; # render Evil cursor shapes inside Herdr panes
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
        # B opens the picker (press `b` for blocked); BTab is the attention command below.
        open_notification_target = "prefix+o";
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
          # tmux paste-buffer habit. herdr has no paste buffer, so paste the system clipboard.
          {
            key = "prefix+]";
            type = "shell";
            description = "paste clipboard";
            command = "${cfg.pasteCommand} | ${launch "paste"}";
          }
          # Yank the active pane's directory. xclip lingers to serve the selection, so detach its output.
          {
            key = "prefix+shift+y";
            type = "shell";
            description = "copy pane directory";
            command = ''dir=''${HERDR_ACTIVE_PANE_CWD:-$PWD}; printf '%s' "$dir" | ${cfg.copyCommand} >/dev/null 2>&1 && "$HERDR_BIN_PATH" notification show "Copied $dir" >/dev/null'';
          }
          # Project workspace from a recency-ranked fzf list (zoxide, then projectRoots).
          {
            key = "prefix+f";
            type = "popup";
            description = "open project workspace";
            width = "80%";
            height = "60%";
            command = launch "project";
          }
          # New agent in its own git worktree, grouped under the active repo.
          {
            key = "prefix+shift+a";
            type = "popup";
            description = "agent in new worktree";
            width = "60%";
            height = "40%";
            command = launch "agent ${lib.escapeShellArgs (map (k: lib.concatStringsSep " " ([k] ++ cfg.agentArgs.${k} or [])) cfg.agentKinds)}";
          }
          # Start an asciinema-recorded shell in a new tab (prompt for a title).
          # Exit the shell to finalize the cast; render it with @webframp/screencast.
          {
            key = "prefix+shift+s";
            type = "popup";
            description = "record a session (asciinema)";
            width = "60%";
            height = "20%";
            command = record "start";
          }
          # tmux-jump-bell replacement: latest blocked agent, else latest finished one.
          # open_notification_target only sees in-app toasts, which ui.toast.delivery skips.
          {
            key = "prefix+shift+tab";
            type = "shell";
            description = "jump to agent needing attention";
            command = launch "attention";
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

    # tmux automatic-rename has no herdr equivalent, so zsh hooks rename the tab.
    programs.zsh.initContent = lib.mkIf cfg.autoRenameTabs (
      builtins.replaceStrings ["@herdr_launch@"] [(lib.getExe herdr-launch)]
      (builtins.readFile ../../home/sme/shared/includes/herdr-tab-name.zsh)
    );

    xdg.configFile."herdr/config.toml".source = toml.generate "herdr-config.toml" cfg.settings;
  };
}
