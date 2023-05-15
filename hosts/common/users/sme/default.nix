{ pkgs, config, ... }:
let
  ifTheyExist = groups:
    builtins.filter (group: builtins.hasAttr group config.users.groups) groups;
in {
  users.users.sme = {
    isNormalUser = true;
    # shell = pkgs.zsh;
    extraGroups = [ "wheel" "video" "audio" ]
      ++ ifTheyExist [ "network" "wireshark" "docker" "podman" "git" ];
  };

  home-manager.users.sme =
    import ../../../../home/sme/${config.networking.hostName}.nix;
}
