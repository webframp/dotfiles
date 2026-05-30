{pkgs, ...}: let
  gpgKey = "BE06ADB38C7F719D";
in {
  imports = [
    ./shared/base.nix
    ./shared/linux.nix
  ];

  home.shellAliases = {
    yay = "/run/wrappers/bin/sudo nixos-rebuild switch --flake .#galvatron";
    # WSLg Wayland clipboard bridge corrupts non-ASCII; force X11 fallback
    # aka: mojibake
    kiro-cli = "WAYLAND_DISPLAY= command kiro-cli";
  };

  webframp.tmux.copyCommand = "win32yank.exe -i";

  programs.keychain = {
    enable = true;
    enableZshIntegration = true;
    keys = ["id_ed25519" gpgKey];
    extraFlags = ["--nogui" "--quiet"];
  };

  programs.gpg = {
    enable = true;
    settings = {
      default-key = gpgKey;
    };
  };

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 7776000; # 90 days
    maxCacheTtl = 7776000;
    pinentry.package = pkgs.pinentry-curses;
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacs-pgtk;
    extraPackages = epkgs: [
      epkgs.mu4e
      epkgs.vterm
    ];
  };

  webframp.mail = {
    enable = true;
    accounts.proton = {
      address = "webframp@protonmail.com";
      primary = true;
      passEntry = "email/protonmail.com/bridge";
      gpgKey = gpgKey;
      aliases = [
        "sean@webframp.com"
        "me@webframp.com"
        "web@webframp.com"
      ];
    };
  };
}
