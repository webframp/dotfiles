{inputs, ...}: {
  imports = [
    ./global
    ./linux.nix
  ];

  home.shellAliases = {
    yay = "/run/wrappers/bin/sudo nixos-rebuild switch --flake .#galvatron";
  };

  # TODO these files need to exist but it's manual for now
  programs.keychain = {
    enable = true;
    enableZshIntegration = true;
    keys = ["id_ed25519" "BE06ADB38C7F719D"];
  };
}
