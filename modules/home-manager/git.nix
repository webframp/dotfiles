# ABOUTME: Shared git configuration module
# ABOUTME: Provides consistent git settings, aliases, and signing across hosts
{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.webframp.git;
  # gpg wrapper for non-interactive AI-agent sessions (Claude Code, etc.).
  # A cold gpg-agent cache would otherwise launch pinentry-curses to prompt
  # for the signing passphrase; in an agent-driven shell no human answers it,
  # the curses UI seizes the tty, and the session wedges. --pinentry-mode error
  # makes gpg fail fast instead: a warm cache still signs silently, a cold one
  # returns a clean error so the agent reports failure rather than hanging.
  # Interactive shells (no CLAUDECODE/AI_AGENT) fall through to plain gpg and
  # keep prompting via pinentry as usual.
  gpgForAgents = pkgs.writeShellScript "gpg-agent-safe" ''
    if [ -n "$CLAUDECODE" ] || [ -n "$AI_AGENT" ]; then
      exec ${pkgs.gnupg}/bin/gpg --pinentry-mode error "$@"
    fi
    exec ${pkgs.gnupg}/bin/gpg "$@"
  '';
in {
  options.webframp.git = {
    enable = mkEnableOption "custom git configuration";

    credentialHelper = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = "Git credential helper command (e.g., pass-git-helper)";
    };

    # TODO: explore sops-nix or agenix for managing forge credentials declaratively
    extraIncludes = mkOption {
      type = types.listOf types.attrs;
      default = [];
      description = "Additional git include directives (e.g., for forge instance configs)";
    };
  };

  config = mkIf cfg.enable {
    programs.git = {
      enable = true;
      includes = cfg.extraIncludes;
      signing.format = "openpgp";
      signing.key = "BE06ADB38C7F719D";
      settings = {
        gpg.program = "${gpgForAgents}";
        user.name = "Sean Escriva";
        user.email = "sean.escriva@gmail.com";
        alias = {
          "in" = "log ..@{upstream}";
          out = "log @{upstream}..";
          st = "status";
          co = "checkout";
          ci = "commit";
          br = "branch";
          r = "reset";
          rh = "reset --hard";
          rh1 = "reset --hard HEAD~1";
          rh2 = "reset --hard HEAD~2";
          stage = "add -p";
          ls = "log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --date=relative";
          ll = "log --pretty=format:'%Cred%h%C(yellow)%d%Creset -%Creset %s %Cgreen(%cr) %C(bold blue)<%cn>%Creset' --decorate --numstat";
          dlc = "diff --cached HEAD^";
          filelog = "log -u";
          fl = "log -u";
          serve = "daemon --reuseaddr --base-path=. --export-all --verbose";
          pos = "push -o ci.skip";
          pmr = "push origin HEAD --force-with-lease -o merge_request.remove_source_branch -o merge_request.create";
        };
        branch = {
          autosetuprebase = "always";
          sort = "committerdate";
        };
        color = {ui = true;};
        column = {ui = "auto";};
        core = {
          excludesfile = "~/.gitignore";
          attributesfile = "~/.gitattributes";
        };
        credential = mkIf (cfg.credentialHelper != null) {
          helper = cfg.credentialHelper;
          useHttpPath = true;
        };
        diff = {
          algorithm = "histogram";
          colorMoved = "plain";
          mnemonicprefix = true;
          renames = true;
        };
        fetch = {
          all = true;
          prune = true;
          prunetags = true;
        };
        github = {user = "webframp";};
        gitlab = {user = "webframp";};
        help = {autocorrect = 1;};
        init = {defaultBranch = "main";};
        merge = {conflictStyle = "zdiff3";};
        protocol = {version = 2;};
        pull = {rebase = true;};
        push = {
          autosetupremote = true;
          default = "simple";
          followtags = true;
        };
        rebase = {
          autosquash = true;
          autostash = true;
          updaterefs = true;
        };
        rerere = {
          enabled = true;
          autoupdate = true;
        };
        tag = {sort = "version:refname";};
      };
    };
  };
}
