{
  config,
  lib,
  pkgs,
  modulesPath,
  ...
}: {
  imports = [];

  boot.initrd.availableKernelModules = ["virtio_pci"];
  boot.initrd.kernelModules = [];
  boot.kernelModules = ["kvm-amd"];
  boot.extraModulePackages = [];

  fileSystems."/" = {
    device = "/dev/sdd";
    fsType = "ext4";
  };

  fileSystems."/usr/lib/wsl/drivers" = {
    device = "drivers";
    fsType = "9p";
  };

  fileSystems."/usr/lib/wsl/lib" = {
    device = "lib";
    fsType = "9p";
  };

  fileSystems."/mnt/wsl" = {
    device = "none";
    fsType = "tmpfs";
  };

  fileSystems."/mnt/wslg" = {
    device = "none";
    fsType = "tmpfs";
  };

  # /mnt/wslg/doc is an overlay mount that WSLg sets up itself at boot with
  # its own lowerdir/upperdir/workdir. A static fstab "overlay/defaults" line
  # cannot reproduce it (no overlay options => "wrong fs type/bad superblock"),
  # so declaring it here only produced a failed mount unit and a non-zero
  # switch-to-configuration exit. Leave it to WSLg.

  fileSystems."/mnt/c" = {
    device = "drvfs";
    fsType = "9p";
  };

  fileSystems."/mnt/d" = {
    device = "drvfs";
    fsType = "9p";
  };

  swapDevices = [];

  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = lib.mkDefault true;
  # networking.interfaces.bond0.useDHCP = lib.mkDefault true;
  # networking.interfaces.bonding_masters.useDHCP = lib.mkDefault true;
  # networking.interfaces.dummy0.useDHCP = lib.mkDefault true;
  # networking.interfaces.eth0.useDHCP = lib.mkDefault true;
  # networking.interfaces.sit0.useDHCP = lib.mkDefault true;
  # networking.interfaces.tunl0.useDHCP = lib.mkDefault true;

  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  # Set your system kind (needed for flakes)
  nixpkgs.hostPlatform = "x86_64-linux";
}
