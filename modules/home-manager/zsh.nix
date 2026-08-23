# ABOUTME: Reusable zsh configuration module for home-manager
# ABOUTME: Provides shared zsh setup with options for vterm, platform-specific settings, and plugin management
{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.webframp.zsh;
in {
  options.webframp.zsh = {
    enable = mkEnableOption "custom zsh configuration";

    enableVterm = mkOption {
      type = types.bool;
      default = false;
      description = "Enable Emacs vterm shell integration";
    };

    promptTheme = mkOption {
      type = types.enum ["p10k" "simple"];
      default = "p10k";
      description = ''
        Which prompt to use. "p10k" enables Powerlevel10k (instant prompt,
        zplug theme, and ~/.p10k.zsh). "simple" uses a minimal built-in
        zsh prompt (path + git branch via vcs_info, no extra plugin/forks)
        for comparing startup performance without p10k in the loop.
      '';
    };

    extraZplugPlugins = mkOption {
      type = types.listOf types.attrs;
      default = [];
      description = "Additional zplug plugins to include beyond the base set";
    };

    extraShellAliases = mkOption {
      type = types.attrsOf types.str;
      default = {};
      description = "Additional shell aliases to merge with base aliases";
    };

    extraEnvVars = mkOption {
      type = types.lines;
      default = "";
      description = "Additional environment variables for envExtra";
    };

    extraInitContent = mkOption {
      type = types.lines;
      default = "";
      description = "Additional init content to append after base config";
    };
  };

  config = mkIf cfg.enable {
    # Enable shell integration for supported tools (direnv, zoxide, etc.)
    home.shell.enableShellIntegration = true;

    # Environment variables shared across all hosts
    home.sessionVariables = {
      AWS_VAULT_BACKEND = "pass";
    };

    # Override .zprofile so Kiro CLI pre block is placed first
    home.file = mkMerge [
      {
        ".zprofile".text = mkForce ''
          # Kiro CLI pre block. Keep at the top of this file.
          [[ -f "''${HOME}/Library/Application Support/kiro-cli/shell/zprofile.pre.zsh" ]] && builtin source "''${HOME}/Library/Application Support/kiro-cli/shell/zprofile.pre.zsh"

          # Environment variables
          . "${config.home.profileDirectory}/etc/profile.d/hm-session-vars.sh"

          # Only source this once
          if [[ -z "''${__HM_ZSH_SESS_VARS_SOURCED-}" ]]; then
            export __HM_ZSH_SESS_VARS_SOURCED=1
          fi

          WORDCHARS='*?[]~=&;!#$%^(){}<>'

          # Kiro CLI post block. Keep at the bottom of this file.
          [[ -f "''${HOME}/Library/Application Support/kiro-cli/shell/zprofile.post.zsh" ]] && builtin source "''${HOME}/Library/Application Support/kiro-cli/shell/zprofile.post.zsh"
        '';
      }
      # Powerlevel10k configuration file
      (mkIf (cfg.promptTheme == "p10k") {
        ".p10k.zsh".source = ../../home/sme/shared/includes/p10k.zsh;
      })
    ];

    programs.zsh = {
      enable = true;
      enableCompletion = true;
      autocd = true;

      history = {
        expireDuplicatesFirst = true;
        extended = true;
        ignoreDups = true;
        ignoreSpace = true;
        size = 100000;
        save = 100000;
      };

      envExtra = ''
        export JSII_SILENCE_WARNING_UNTESTED_NODE_VERSION=true
        export GPG_TTY=''${GPG_TTY:-$(tty 2>/dev/null)}
        ${cfg.extraEnvVars}
      '';

      # Cache compinit - only regenerate once per day
      completionInit = ''
        autoload -Uz compinit
        if [[ -n ''${ZDOTDIR:-$HOME}/.zcompdump(#qN.mh+24) ]]; then
          compinit
        else
          compinit -C
        fi
      '';

      loginExtra = ''
        # Compile zcompdump in background to increase startup speed
        {
            zcompdump="''${ZDOTDIR:-$HOME}/.zcompdump"
            if [[ -s "$zcompdump" && (! -s "''${zcompdump}.zwc" || "$zcompdump" -nt "''${zcompdump}.zwc") ]]; then
                zcompile "$zcompdump"
            fi
        } &!
      '';

      # Use mkMerge with mkOrder to control content placement
      # See: https://github.com/nix-community/home-manager/pull/6479
      initContent = let
        kiroPreBlock = mkOrder 50 ''
          # Kiro CLI pre block. Keep at the top of this file.
          [[ -f "''${HOME}/Library/Application Support/kiro-cli/shell/zshrc.pre.zsh" ]] && builtin source "''${HOME}/Library/Application Support/kiro-cli/shell/zshrc.pre.zsh"
        '';

        # Priority 100: p10k instant prompt must be at very top (skip in vterm)
        p10kInstantPrompt = mkOrder 100 (optionalString (cfg.promptTheme == "p10k") ''
          if [[ "$INSIDE_EMACS" != 'vterm' ]] && [[ -r "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh" ]]; then
            source "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh"
          fi
        '');

        vtermIntegration = ''
          # Emacs vterm integration
          # https://github.com/akermu/emacs-libvterm#shell-side-configuration
          vterm_printf() {
              if [ -n "$TMUX" ] && ([ "''${TERM%%-*}" = "tmux" ] || [ "''${TERM%%-*}" = "screen" ]); then
                  printf "\ePtmux;\e\e]%s\007\e\\" "$1"
              elif [ "''${TERM%%-*}" = "screen" ]; then
                  printf "\eP\e]%s\007\e\\" "$1"
              else
                  printf "\e]%s\e\\" "$1"
              fi
          }

          vterm_prompt_end() {
              vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
          }

          vterm_cmd() {
              local vterm_elisp
              vterm_elisp=""
              while [ $# -gt 0 ]; do
                  vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
                  shift
              done
              vterm_printf "51;E$vterm_elisp"
          }

          if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
              # Override clear to work properly in vterm
              alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
              # Directory tracking - append to PROMPT
              PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'
          fi
        '';

        baseConfig = ''
          # Calculator using zsh's builtin zcalc
          autoload -U zcalc
          __calc_fn() { zcalc -f -e "$*" }
          alias calc="noglob __calc_fn"

          # Pass aliases for password store with fzf integration.
          # WAYLAND_DISPLAY= forces pass's clip() down the xclip/X11 branch:
          # WSLg exports WAYLAND_DISPLAY, which makes pass reach for wl-copy
          # (not installed, and its bridge mangles non-ASCII). Empty on non-WSL.
          alias qp='WAYLAND_DISPLAY= pass -c "$(find -L "$HOME/.password-store" \( -name .git\* -o -name .gpg-id \) -prune -o $@ -print 2>/dev/null | sed -e "s#''${HOME}/.password-store/\{0,1\}##" -e 's#\.gpg##'|sort|fzf)"'
          alias qpo='WAYLAND_DISPLAY= pass otp -c "$(find -L "$HOME/.password-store" \( -name .git\* -o -name .gpg-id \) -prune -o $@ -print 2>/dev/null | sed -e "s#''${HOME}/.password-store/\{0,1\}##" -e 's#\.gpg##'|sort|fzf)"'
          alias qpe='EDITOR=vim pass edit "$(find -L "$HOME/.password-store" \( -name .git\* -o -name .gpg-id \) -prune -o $@ -print 2>/dev/null | sed -e "s#''${HOME}/.password-store/\{0,1\}##" -e 's#\.gpg##'|sort|fzf)"'

          # zsh-autosuggestions keybind
          bindkey '^ ' autosuggest-accept

          # zsh-autosuggestions unconditionally re-enables its async mode at
          # plugin-load time (it only checks whether this var is *set*, not
          # its value, and sets it itself for any zsh >= 5.0.8). Async mode
          # forks a subprocess per keystroke to compute the suggestion, which
          # is expensive enough on WSL2 to be felt as input lag. Since this
          # check is dynamic (re-evaluated on every keystroke, not just once
          # at load), unsetting it here — after zplug has already loaded the
          # plugin and made its own assignment — sticks for the rest of the
          # session. Must not be set in envExtra/.zshenv: that runs before
          # zplug load and gets overwritten right back.
          unset ZSH_AUTOSUGGEST_USE_ASYNC

          # fast-syntax-highlighting's git/docker/hub/lab chromas shell out on
          # every keystroke (5s cache) to colorize subcommands/branches/refs.
          # WSL2 process-spawn overhead makes that a visible stall when you
          # start typing those commands. Disable just those chromas; the rest
          # of the highlighter (commands, strings, brackets, etc.) stays fast.
          unset "FAST_HIGHLIGHT[chroma-git]" "FAST_HIGHLIGHT[chroma-docker]" \
                "FAST_HIGHLIGHT[chroma-hub]" "FAST_HIGHLIGHT[chroma-lab]"

          ${
            if cfg.promptTheme == "p10k"
            then ''
              # powerlevel10k config
              [[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
            ''
            else ''
              # Minimal prompt: cwd + git branch, no theme engine/plugin overhead.
              autoload -Uz vcs_info
              precmd_functions+=( vcs_info )
              zstyle ':vcs_info:git:*' formats ' (%b)'
              setopt PROMPT_SUBST
              PROMPT='%B%F{blue}%~%f%b%F{green}''${vcs_info_msg_0_}%f %# '
            ''
          }

          ${cfg.extraInitContent}
        '';

        kiroPostBlock = mkOrder 2000 ''
          # Kiro CLI post block. Keep at the bottom of this file.
          [[ -f "''${HOME}/Library/Application Support/kiro-cli/shell/zshrc.post.zsh" ]] && builtin source "''${HOME}/Library/Application Support/kiro-cli/shell/zshrc.post.zsh"
        '';
      in
        mkMerge [
          kiroPreBlock
          p10kInstantPrompt
          ((optionalString cfg.enableVterm vtermIntegration) + baseConfig)
          kiroPostBlock
        ];

      zplug = {
        enable = true;
        plugins =
          [
            # Base plugins - forks maintained by webframp
            {name = "webframp/zsh-async";}
            {
              name = "webframp/zsh-completions";
              tags = ["defer:0"];
            }
            {
              name = "webframp/zsh-autosuggestions";
              tags = ["defer:2" "on:'webframp/zsh-completions'"];
            }
            {
              name = "webframp/fast-syntax-highlighting";
              tags = ["defer:3" "on:'webframp/zsh-autosuggestions'"];
            }
          ]
          ++ optional (cfg.promptTheme == "p10k") {
            name = "webframp/powerlevel10k";
            tags = ["as:theme" "depth:1"];
          }
          ++ cfg.extraZplugPlugins;
      };
    };

    # Base shell aliases shared across all hosts
    home.shellAliases =
      {
        # Directory listing with eza
        lls = "${pkgs.eza}/bin/eza --color=auto --group-directories-first --classify";
        lll = "${pkgs.eza}/bin/eza --color=auto --group-directories-first --classify --all --long --header --group";
        cdtemp = "cd `mktemp -df`";

        # Git shortcuts
        gst = "git status";
        gpo = "git push origin HEAD";
        gpu = "git pull --prune --tags --all";
        repo = "git browse >/dev/null";

        # SSH with proper TERM
        ssh = "TERM=xterm-256color ssh";

        # Shell reload
        reload = "exec $SHELL -l";
      }
      // cfg.extraShellAliases;
  };
}
