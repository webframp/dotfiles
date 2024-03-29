{inputs, ...}: {
  imports = [./global];

  home.shellAliases = {
    yay = "sudo nixos-rebuild switch --flake .#galvatron";
  };

  # TODO these files need to exist but it's manual for now
  programs.keychain = {
    enable = true;
    enableZshIntegration = true;
    agents = ["ssh" "gpg"];
    keys = ["id_ed25519" "BE06ADB38C7F719D"];
  };
}
