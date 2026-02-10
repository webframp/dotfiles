{pkgs, ...}: let
  gpgKey = "BE06ADB38C7F719D";
in {
  imports = [
    ./shared/base.nix
    ./shared/linux.nix
  ];

  home.shellAliases = {
    yay = "/run/wrappers/bin/sudo nixos-rebuild switch --flake .#galvatron";
  };

  programs.gpg = {
    enable = true;
    settings = {
      default-key = gpgKey;
      no-tty = true;
      use-agent = true;
    };
  };

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 84000;
    maxCacheTtl = 84000;
    pinentry.package = pkgs.pinentry-curses;
  };

  programs.keychain = {
    enable = true;
    enableZshIntegration = true;
    extraFlags = ["--nogui" "--noask" "--quiet"];
    keys = ["id_ed25519" gpgKey];
  };
}
