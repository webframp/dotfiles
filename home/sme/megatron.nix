# This is your home-manager configuration file
# Use this to configure your home environment (it replaces ~/.config/nixpkgs/home.nix)

{ inputs, lib, config, pkgs, ... }: {
  # You can import other home-manager modules here
  imports = [
    # If you want to use home-manager modules from other flakes (such as nix-colors):
    # inputs.nix-colors.homeManagerModule

    # You can also split up your configuration and import pieces of it here:
    # ./nvim.nix
  ];

  nixpkgs = {
    # You can add overlays here
    overlays = [
      # If you want to use overlays exported from other flakes:
      # neovim-nightly-overlay.overlays.default

      # Or define it inline, for example:
      # (final: prev: {
      #   hi = final.hello.overrideAttrs (oldAttrs: {
      #     patches = [ ./change-hello-to-hi.patch ];
      #   });
      # })
    ];
    # Configure your nixpkgs instance
    config = {
      # Disable if you don't want unfree packages
      allowUnfree = true;
      # Workaround for https://github.com/nix-community/home-manager/issues/2942
      allowUnfreePredicate = (_: true);
    };
  };

  home = {
    username = "sme";
    homeDirectory = "/Users/sme";
  };

  # Add stuff for your user as you see fit:
  # programs.neovim.enable = true;
  home.packages = with pkgs; [ tmux ];

  # Enable home-manager and git
  programs.home-manager.enable = true;

  programs.git = {
    enable = true;
    # delta a better pager: https://github.com/dandavison/delta
    delta.enable = true;
    delta.options = {
      navigate = true;
      fatures = "side-by-side line-numbers decorations";
      syntax-theme = "base16-256";
    };

    aliases = {
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
      ls =
        "log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --date=relative";
      ll =
        "log --pretty=format:'%Cred%h%C(yellow)%d%Creset -%Creset %s %Cgreen(%cr) %C(bold blue)<%cn>%Creset' --decorate --numstat";
      dlc = "diff --cached HEAD^";
      filelog = "log -u";
      fl = "log -u";
      serve = "daemon --reuseaddr --base-path=. --export-all --verbose";
      pos = "push -o ci.skip";
      pmr =
        "push origin HEAD --force-with-lease -o merge_request.remove_source_branch -o merge_request.create";
    };

    extraConfig = {
      # [user]
      #   name = Sean Escriva
      #   email = sean.escriva@gmail.com
      #   signingkey = BE06ADB38C7F719D
      # [color]
      #   ui = true
      # [branch]
      #   autosetuprebase = always
      # [init]
      #   defaultBranch = main
      # [core]
      #   excludesfile = ~/.gitignore_global
      #   attributesfile = ~/.gitattributes
      #   editor = ~/bin/EDITOR
      #   pager = delta
      # [interactive]
      #   diffFilter = delta --color-only
      # [merge]
      #   conflictStyle = diff3
      # [diff]
      #   colorMoved = default
      # [push]
      #   default = simple
      # [help]
      #   autocorrect = 1
      # [commit]
      #   gpgsign = true
      # [filter "lfs"]
      #   clean = git-lfs clean -- %f
      #   smudge = git-lfs smudge -- %f
      #   process = git-lfs filter-process
      #   required = true
      # [github]
      #   user = webframp
      # [gitlab]
      #   user = webframp
      # [gitlab "git.bethelservice.org/api/v4"]
      #   user = sescriva
      # [protocol]
      #     version = 2
      # [credential]
      #     helper = osxkeychain # should be per platform value
      # # For work related paths, conditionally adjust git config
      # [includeIf "gitdir:~/src/o11n/"]
      #   path = ~/.work.gitconfig
      # [includeIf "gitdir:~/src/Orchestration/"]
      #   path = ~/.work.gitconfig
      # [includeIf "gitdir:~/src/appsvc/"]
      #   path = ~/.work.gitconfig
      # [includeIf "gitdir:~/src/Redmine-plugins/"]
      #   path = ~/.work.gitconfig
    };
  };

  programs.direnv.enable = true;

  programs.tmux = {
    enable = true;
    shortcut = "j";
    baseIndex = 1;
    escapeTime = 1;

    # Force tmux to use /tmp for sockets (WSL2 compat)
    # secureSocket = false;
    clock24 = true;
    plugins = with pkgs; [
      tmuxPlugins.better-mouse-mode
      tmuxPlugins.continuum
      tmuxPlugins.extrakto # prefix + tab
      tmuxPlugins.fzf-tmux-url
      # tmuxPlugins.nord
      tmuxPlugins.onedark-theme
      tmuxPlugins.pain-control
      tmuxPlugins.resurrect
      tmuxPlugins.sensible
      tmuxPlugins.tmux-thumbs # prefix + I
      tmuxPlugins.yank
    ];
    # TODO the 24bit color config depends on custom terminfo file
    # find a way to build this for systemwide config with nix
    # https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/config/terminfo.nix maybe?
    extraConfig = ''
      # Use 24bit colors if possible or fallback to 256
      # https://old.reddit.com/r/tmux/comments/mesrci/tmux_2_doesnt_seem_to_use_256_colors/
      # https://github.com/syl20bnr/spacemacs/wiki/Terminal

      if -b 'test $TERM = xterm-24bit' 'set -g default-terminal "xterm-24bit"' 'set -g default-terminal "xterm-256color"'
      if -b 'test $TERM = xterm-24bit' 'set -g terminal-overrides ",xterm-24bit:Tc"' 'set -ga terminal-overrides ",*256col*:Tc"'
      if -b 'test $TERM = xterm-24bit' 'set -ga terminal-overrides "*:Ss=\E[%p1%d q:Se=\E[ q"'
      if -b 'test $TERM = xterm-24bit' 'set-environment -g COLORTERM "truecolor"'

      # Mouse enabled
      set-option -g mouse on

      # copy to X11 clipboard
      if -b 'command -v xsel > /dev/null 2>&1' 'bind y run -b "tmux save-buffer - | xsel -i -b"'
      if -b '! command -v xsel > /dev/null 2>&1 && command -v xclip > /dev/null 2>&1' 'bind y run -b "tmux save-buffer - | xclip -i -selection clipboard >/dev/null 2>&1"'
      # copy to Wayland clipboard
      if -b 'command -v wl-copy > /dev/null 2>&1' 'bind y run -b "tmux save-buffer - | wl-copy"'
      # copy to macOS clipboard
      if -b 'command -v pbcopy > /dev/null 2>&1' 'bind y run -b "tmux save-buffer - | pbcopy"'
      if -b 'command -v reattach-to-user-namespace > /dev/null 2>&1' 'bind y run -b "tmux save-buffer - | reattach-to-user-namespace pbcopy"'
      # copy to Windows clipboard
      if -b 'command -v clip.exe > /dev/null 2>&1' 'bind y run -b "tmux save-buffer - | clip.exe"'
      if -b '[ -c /dev/clipboard ]' 'bind y run -b "tmux save-buffer - > /dev/clipboard"'

      # buffers
      bind b list-buffers     # list paste buffers
      bind p paste-buffer -p  # paste from the top paste buffer
      bind P choose-buffer    # choose which buffer to paste from

      bind -r C-h previous-window # select previous window
      bind -r C-l next-window     # select next window
    '';
  };

  # Nicely reload system units when changing configs
  # systemd.user.startServices = "sd-switch";

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  home.stateVersion = "23.05";
}
