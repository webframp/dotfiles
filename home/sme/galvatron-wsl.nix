{pkgs, ...}: let
  gpgKey = "BE06ADB38C7F719D";
in {
  imports = [
    ./shared/base.nix
    ./shared/linux.nix
  ];

  home.sessionVariables = {
    # Point SSH_AUTH_SOCK to gpg-agent's SSH socket
    SSH_AUTH_SOCK = "\${XDG_RUNTIME_DIR}/gnupg/S.gpg-agent.ssh";
  };

  home.shellAliases = {
    yay = "/run/wrappers/bin/sudo nixos-rebuild switch --flake .#galvatron";
  };

  programs.keychain = {
    enable = true;
    enableZshIntegration = true;
    keys = [gpgKey];
    extraFlags = ["--nogui" "--quiet"];
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
    enableSshSupport = true;
    defaultCacheTtl = 7776000; # 90 days
    defaultCacheTtlSsh = 7776000;
    maxCacheTtl = 7776000;
    maxCacheTtlSsh = 7776000;
    pinentry.package = pkgs.pinentry-curses;
  };

}
