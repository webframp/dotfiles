# ABOUTME: Reusable zsh configuration module for home-manager
# ABOUTME: Provides shared zsh setup with options for vterm, platform-specific settings, and plugin management
{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.custom.zsh;
in {
  options.custom.zsh = {
    enable = mkEnableOption "custom zsh configuration";

    enableVterm = mkOption {
      type = types.bool;
      default = false;
      description = "Enable Emacs vterm shell integration";
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

    # Multi-shell completion generator
    programs.carapace.enable = true;

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
        export ZSH_AUTOSUGGEST_USE_ASYNC=true
        export JSII_SILENCE_WARNING_UNTESTED_NODE_VERSION=true
        ${cfg.extraEnvVars}
      '';

      profileExtra = ''
        WORDCHARS='*?[]~=&;!#$%^(){}<>'
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

      initContent = let
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
              # Disable p10k instant prompt in vterm (causes display issues)
              POWERLEVEL9K_INSTANT_PROMPT=off
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

          # Pass aliases for password store with fzf integration
          alias qp='pass -c "$(find -L "$HOME/.password-store" \( -name .git\* -o -name .gpg-id \) -prune -o $@ -print 2>/dev/null | sed -e "s#''${HOME}/.password-store/\{0,1\}##" -e 's#\.gpg##'|sort|fzf)"'
          alias qpo='pass otp -c "$(find -L "$HOME/.password-store" \( -name .git\* -o -name .gpg-id \) -prune -o $@ -print 2>/dev/null | sed -e "s#''${HOME}/.password-store/\{0,1\}##" -e 's#\.gpg##'|sort|fzf)"'
          alias qpe='EDITOR=vim pass edit "$(find -L "$HOME/.password-store" \( -name .git\* -o -name .gpg-id \) -prune -o $@ -print 2>/dev/null | sed -e "s#''${HOME}/.password-store/\{0,1\}##" -e 's#\.gpg##'|sort|fzf)"'

          # zsh-autosuggestions keybind
          bindkey '^ ' autosuggest-accept

          # powerlevel10k
          [[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

          ${cfg.extraInitContent}
        '';
      in
        (optionalString cfg.enableVterm vtermIntegration) + baseConfig;

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
            {
              name = "webframp/powerlevel10k";
              tags = ["as:theme" "depth:1"];
            }
          ]
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

    # Powerlevel10k configuration file
    home.file.".p10k.zsh".source = ../../home/sme/global/includes/p10k.zsh;
  };
}
