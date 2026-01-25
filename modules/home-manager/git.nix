# ABOUTME: Shared git configuration module
# ABOUTME: Provides consistent git settings, aliases, and signing across hosts
{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.custom.git;
in {
  options.custom.git = {
    enable = mkEnableOption "custom git configuration";

    credentialHelper = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = "Git credential helper command (e.g., pass-git-helper)";
    };
  };

  config = mkIf cfg.enable {
    programs.git = {
      enable = true;
      signing.key = "BE06ADB38C7F719D";
      settings = {
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
