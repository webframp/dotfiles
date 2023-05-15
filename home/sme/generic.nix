{ lib, ... }: {
  # TODO: create shared global config
  # imports = [ ./global ];
  # Disable impermanence
  home.persistence = lib.mkForce { };
}
