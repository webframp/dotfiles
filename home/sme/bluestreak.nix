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

    # ./global/default.nix
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
    stateVersion = "24.05";
  };
  #
  # macOS specific packages as needed
  home.packages = with pkgs; [tmux];

  # Enable home-manager and git
  programs.home-manager.enable = true;

  home.shellAliases = {
    yay = "home-manager switch --flake .#sme@bluestreak";
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
        opacity = 0.8;
        startup_mode = "Maximized";
	option_as_alt = "OnlyLeft";
      };

      scrolling.history = 10000;
      scrolling.multiplier = 3;
      colors.draw_bold_text_with_bright_colors = true;

      # font = {
      #   normal.family = "Ioesevka Nerd Font Mono";
      #   size = 18.0;
      #   offset.x = 0;
      #   offset.y = 0;
      #   glyph_offset.x = 0;
      #   glyph_offset.y = 0;
      # };

    };
  };
}
