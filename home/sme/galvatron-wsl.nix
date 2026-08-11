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

    # wsl.interop.includePath is disabled (see hosts/galvatron-wsl/configuration.nix)
    # to keep the ~40 slow /mnt/c/... 9p-backed dirs out of $PATH, so the few
    # Windows binaries we do use interactively are referenced by absolute path.
    "clip.exe" = "/mnt/c/Windows/System32/clip.exe";
    "explorer.exe" = "/mnt/c/Windows/explorer.exe";
    "powershell.exe" = "/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe";
  };

  # xclip → WSLg bridges to the Windows clipboard with correct UTF-8, so no
  # win32yank.exe (Windows interop) dependency. See tmux.conf clipboard notes.
  webframp.tmux.copyCommand = "xclip -selection clipboard -i";
  webframp.zsh.extraEnvVars = ''
    [ -f ~/.keychain/$(hostname)-sh ] && source ~/.keychain/$(hostname)-sh
  '';

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
