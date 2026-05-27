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

}
