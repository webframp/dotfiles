# This is your home-manager configuration file
# Use this to configure your home environment (it replaces ~/.config/nixpkgs/home.nix)
{
  config,
  lib,
  pkgs,
  ...
}: {
  # You can import other home-manager modules here
  imports = [
    # If you want to use home-manager modules from other flakes (such as nix-colors):
    # inputs.nix-colors.homeManagerModule

    ./global/default.nix
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
      allowUnfreePredicate = _: true;
    };
  };

  home = {
    username = "sme";
    homeDirectory = "/Users/sme";
  };
  #
  # macOS specific packages as needed
  home.packages = with pkgs; [tmux];

  # Enable home-manager and git
  programs.home-manager.enable = true;

  home.shellAliases = {
    yay = "home-manager switch --flake .#sme@megatron";
    yayb = "brew update && brew upgrade && brew cleanup";
  };

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
      ls = "log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --date=relative";
      ll = "log --pretty=format:'%Cred%h%C(yellow)%d%Creset -%Creset %s %Cgreen(%cr) %C(bold blue)<%cn>%Creset' --decorate --numstat";
      dlc = "diff --cached HEAD^";
      filelog = "log -u";
      fl = "log -u";
      serve = "daemon --reuseaddr --base-path=. --export-all --verbose";
      pos = "push -o ci.skip";
      pmr = "push origin HEAD --force-with-lease -o merge_request.remove_source_branch -o merge_request.create";
    };
  };

  programs.direnv.enable = true;
  programs.alacritty = {
    enable = true;
    settings = {
      window = {
        dimensions = {
          columns = 0;
          lines = 0;
        };
        padding = {
          x = 2;
          y = 2;
        };
        dynamic_padding = false;
        decorations = "buttonless";
        opacity = "0.8";
        startup_mode = "Maximized";
      };
      scrolling.history = 10000;
      scrolling.multiplier = 3;

      # font = {
      #   normal.family = "Ioesevka Nerd Font Mono";
      #   size = 18.0;
      #   offset.x = 0;
      #   offset.y = 0;
      #   glyph_offset.x = 0;
      #   glyph_offset.y = 0;
      # };

      draw_bold_text_with_bright_colors = true;
      # https://github.com/jwilm/alacritty/issues/62
      key_bindings = [
        {
          key = "F";
          mods = "Alt";
          chars = "\x1bf";
        }
        {
          key = "B";
          mods = "Alt";
          chars = "\x1bb";
        }
        {
          key = "D";
          mods = "Alt";
          chars = "\x1bd";
        }
        {
          key = "W";
          mods = "Alt";
          chars = "\x1bw";
        }
        {
          key = "V";
          mods = "Alt";
          chars = "\x1bv";
        }
        {
          key = "V";
          mods = "Command";
          action = "Paste";
        }
        {
          key = "C";
          mods = "Command";
          action = "Copy";
        }
        {
          key = "Q";
          mods = "Command";
          action = "Quit";
        }
        {
          key = "W";
          mods = "Command";
          action = "Quit";
        }
        {
          key = "N";
          mods = "Command";
          action = "SpawnNewInstance";
        }
        {
          key = "Home";
          chars = "\x1bOH";
          mode = "AppCursor";
        }
        {
          key = "Home";
          chars = "\x1b[H";
          mode = "~AppCursor";
        }
        {
          key = "End";
          chars = "\x1bOF";
          mode = "AppCursor";
        }
        {
          key = "End";
          chars = "\x1b[F";
          mode = "~AppCursor";
        }
        {
          key = "Key0";
          mods = "Command";
          action = "ResetFontSize";
        }
        {
          key = "Equals";
          mods = "Command";
          action = "IncreaseFontSize";
        }
        {
          key = "Minus";
          mods = "Command";
          action = "DecreaseFontSize";
        }
        {
          key = "PageUp";
          mods = "Shift";
          chars = "\x1b[5;2~";
        }
        {
          key = "PageUp";
          mods = "Control";
          chars = "\x1b[5;5~";
        }
        {
          key = "PageUp";
          chars = "\x1b[5~";
        }
        {
          key = "PageDown";
          mods = "Shift";
          chars = "\x1b[6;2~";
        }
        {
          key = "PageDown";
          mods = "Control";
          chars = "\x1b[6;5~";
        }
        {
          key = "PageDown";
          chars = "\x1b[6~";
        }
        {
          key = "Tab";
          mods = "Shift";
          chars = "\x1b[Z";
        }
        {
          key = "Back";
          chars = "\x7f";
        }
        {
          key = "Back";
          mods = "Alt";
          chars = "\x1b\x7f";
        }
        {
          key = "Insert";
          chars = "\x1b[2~";
        }
        {
          key = "Delete";
          chars = "\x1b[3~";
        }
        {
          key = "Left";
          mods = "Shift";
          chars = "\x1b[1;2D";
        }
        {
          key = "Left";
          mods = "Control";
          chars = "\x1b[1;5D";
        }
        {
          key = "Left";
          mods = "Alt";
          chars = "\x1b[1;3D";
        }
        {
          key = "Left";
          chars = "\x1b[D";
          mode = "~AppCursor";
        }
        {
          key = "Left";
          chars = "\x1bOD";
          mode = "AppCursor";
        }
        {
          key = "Right";
          mods = "Shift";
          chars = "\x1b[1;2C";
        }
        {
          key = "Right";
          mods = "Control";
          chars = "\x1b[1;5C";
        }
        {
          key = "Right";
          mods = "Alt";
          chars = "\x1b[1;3C";
        }
        {
          key = "Right";
          chars = "\x1b[C";
          mode = "~AppCursor";
        }
        {
          key = "Right";
          chars = "\x1bOC";
          mode = "AppCursor";
        }
        {
          key = "Up";
          mods = "Shift";
          chars = "\x1b[1;2A";
        }
        {
          key = "Up";
          mods = "Control";
          chars = "\x1b[1;5A";
        }
        {
          key = "Up";
          mods = "Alt";
          chars = "\x1b[1;3A";
        }
        {
          key = "Up";
          chars = "\x1b[A";
          mode = "~AppCursor";
        }
        {
          key = "Up";
          chars = "\x1bOA";
          mode = "AppCursor";
        }
        {
          key = "Down";
          mods = "Shift";
          chars = "\x1b[1;2B";
        }
        {
          key = "Down";
          mods = "Control";
          chars = "\x1b[1;5B";
        }
        {
          key = "Down";
          mods = "Alt";
          chars = "\x1b[1;3B";
        }
        {
          key = "Down";
          chars = "\x1b[B";
          mode = "~AppCursor";
        }
        {
          key = "Down";
          chars = "\x1bOB";
          mode = "AppCursor";
        }
        {
          key = "Space";
          mods = "Control";
          chars = "\x00";
        } # Ctrl + Space`
        {
          key = "`";
          mods = "Alt";
          chars = "\x1b`";
        } # Alt + `
        {
          key = "`";
          mods = "Alt|Shift";
          chars = "\x1b~";
        } # Alt + ~
        {
          key = "Period";
          mods = "Alt";
          chars = "\x1b.";
        } # Alt + .
        {
          key = "Key8";
          mods = "Alt|Shift";
          chars = "\x1b*";
        } # Alt + *
        {
          key = "Key3";
          mods = "Alt|Shift";
          chars = "\x1b#";
        } # Alt + #
        {
          key = "Period";
          mods = "Alt|Shift";
          chars = "\x1b>";
        } # Alt + >
        {
          key = "Comma";
          mods = "Alt|Shift";
          chars = "\x1b<";
        } # Alt + <
        {
          key = "Minus";
          mods = "Alt|Shift";
          chars = "\x1b_";
        } # Alt + _
        {
          key = "Key5";
          mods = "Alt|Shift";
          chars = "\x1b%";
        } # Alt + %
        {
          key = "Key6";
          mods = "Alt|Shift";
          chars = "\x1b^";
        } # Alt + ^
        {
          key = "Backslash";
          mods = "Alt";
          chars = "\x1b\\";
        } # Alt + \
        {
          key = "Backslash";
          mods = "Alt|Shift";
          chars = "\x1b|";
        } # Alt + |
        {
          key = "F1";
          chars = "\x1bOP";
        }
        {
          key = "F2";
          chars = "\x1bOQ";
        }
        {
          key = "F3";
          chars = "\x1bOR";
        }
        {
          key = "F4";
          chars = "\x1bOS";
        }
        {
          key = "F5";
          chars = "\x1b[15~";
        }
        {
          key = "F6";
          chars = "\x1b[17~";
        }
        {
          key = "F7";
          chars = "\x1b[18~";
        }
        {
          key = "F8";
          chars = "\x1b[19~";
        }
        {
          key = "F9";
          chars = "\x1b[20~";
        }
        {
          key = "F10";
          chars = "\x1b[21~";
        }
        {
          key = "F11";
          chars = "\x1b[23~";
        }
        {
          key = "F12";
          chars = "\x1b[24~";
        }
        {
          key = "F1";
          mods = "Shift";
          chars = "\x1b[1;2P";
        }
        {
          key = "F2";
          mods = "Shift";
          chars = "\x1b[1;2Q";
        }
        {
          key = "F3";
          mods = "Shift";
          chars = "\x1b[1;2R";
        }
        {
          key = "F4";
          mods = "Shift";
          chars = "\x1b[1;2S";
        }
        {
          key = "F5";
          mods = "Shift";
          chars = "\x1b[15;2~";
        }
        {
          key = "F6";
          mods = "Shift";
          chars = "\x1b[17;2~";
        }
        {
          key = "F7";
          mods = "Shift";
          chars = "\x1b[18;2~";
        }
        {
          key = "F8";
          mods = "Shift";
          chars = "\x1b[19;2~";
        }
        {
          key = "F9";
          mods = "Shift";
          chars = "\x1b[20;2~";
        }
        {
          key = "F10";
          mods = "Shift";
          chars = "\x1b[21;2~";
        }
        {
          key = "F11";
          mods = "Shift";
          chars = "\x1b[23;2~";
        }
        {
          key = "F12";
          mods = "Shift";
          chars = "\x1b[24;2~";
        }
        {
          key = "F1";
          mods = "Control";
          chars = "\x1b[1;5P";
        }
        {
          key = "F2";
          mods = "Control";
          chars = "\x1b[1;5Q";
        }
        {
          key = "F3";
          mods = "Control";
          chars = "\x1b[1;5R";
        }
        {
          key = "F4";
          mods = "Control";
          chars = "\x1b[1;5S";
        }
        {
          key = "F5";
          mods = "Control";
          chars = "\x1b[15;5~";
        }
        {
          key = "F6";
          mods = "Control";
          chars = "\x1b[17;5~";
        }
        {
          key = "F7";
          mods = "Control";
          chars = "\x1b[18;5~";
        }
        {
          key = "F8";
          mods = "Control";
          chars = "\x1b[19;5~";
        }
        {
          key = "F9";
          mods = "Control";
          chars = "\x1b[20;5~";
        }
        {
          key = "F10";
          mods = "Control";
          chars = "\x1b[21;5~";
        }
        {
          key = "F11";
          mods = "Control";
          chars = "\x1b[23;5~";
        }
        {
          key = "F12";
          mods = "Control";
          chars = "\x1b[24;5~";
        }
        {
          key = "F1";
          mods = "Alt";
          chars = "\x1b[1;6P";
        }
        {
          key = "F2";
          mods = "Alt";
          chars = "\x1b[1;6Q";
        }
        {
          key = "F3";
          mods = "Alt";
          chars = "\x1b[1;6R";
        }
        {
          key = "F4";
          mods = "Alt";
          chars = "\x1b[1;6S";
        }
        {
          key = "F5";
          mods = "Alt";
          chars = "\x1b[15;6~";
        }
        {
          key = "F6";
          mods = "Alt";
          chars = "\x1b[17;6~";
        }
        {
          key = "F7";
          mods = "Alt";
          chars = "\x1b[18;6~";
        }
        {
          key = "F8";
          mods = "Alt";
          chars = "\x1b[19;6~";
        }
        {
          key = "F9";
          mods = "Alt";
          chars = "\x1b[20;6~";
        }
        {
          key = "F10";
          mods = "Alt";
          chars = "\x1b[21;6~";
        }
        {
          key = "F11";
          mods = "Alt";
          chars = "\x1b[23;6~";
        }
        {
          key = "F12";
          mods = "Alt";
          chars = "\x1b[24;6~";
        }
        {
          key = "F1";
          mods = "Command";
          chars = "\x1b[1;3P";
        }
        {
          key = "F2";
          mods = "Command";
          chars = "\x1b[1;3Q";
        }
        {
          key = "F3";
          mods = "Command";
          chars = "\x1b[1;3R";
        }
        {
          key = "F4";
          mods = "Command";
          chars = "\x1b[1;3S";
        }
        {
          key = "F5";
          mods = "Command";
          chars = "\x1b[15;3~";
        }
        {
          key = "F6";
          mods = "Command";
          chars = "\x1b[17;3~";
        }
        {
          key = "F7";
          mods = "Command";
          chars = "\x1b[18;3~";
        }
        {
          key = "F8";
          mods = "Command";
          chars = "\x1b[19;3~";
        }
        {
          key = "F9";
          mods = "Command";
          chars = "\x1b[20;3~";
        }
        {
          key = "F10";
          mods = "Command";
          chars = "\x1b[21;3~";
        }
        {
          key = "F11";
          mods = "Command";
          chars = "\x1b[23;3~";
        }
        {
          key = "F12";
          mods = "Command";
          chars = "\x1b[24;3~";
        }
      ];
    };
  };
}
