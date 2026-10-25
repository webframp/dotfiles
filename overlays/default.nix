# This file defines overlays
{inputs, ...}: {
  # This one brings our custom packages from the 'pkgs' directory
  additions = final: _prev: import ../pkgs {pkgs = final;};

  # This one contains whatever you want to overlay
  # You can change versions, add patches, set compilation flags, anything really.
  # https://nixos.wiki/wiki/Overlays
  modifications = final: prev: {
    # pipx 1.14.0's test_inject.py uses comma-separated parametrize values that
    # pytest 9 rejects at collection time, before the -k deselects can apply.
    pipx = prev.pipx.overrideAttrs (oldAttrs: {
      disabledTestPaths = (oldAttrs.disabledTestPaths or []) ++ ["tests/test_inject.py"];
    });

    # fabric 3.2.3 declares invoke<3 and is marked broken once invoke reached
    # 3.0.3 on nixpkgs-unstable (NixOS/nixpkgs#575989, fabric/fabric#2376), so
    # azure-cli (a fabric dependent) fails to even evaluate. Fix it at the
    # fabric level, mirroring the accepted upstream fix NixOS/nixpkgs#576874:
    # use the unreleased fabric snapshot that supports invoke 3 and drop the
    # broken guard. Applied via pythonPackagesExtensions (not a top-level
    # python3Packages override) because azure-cli builds its own python set via
    # python3.override, which replaces packageOverrides -- only
    # pythonPackagesExtensions composes through that and reaches py.pkgs.fabric.
    # Keeps azure-cli at its current unstable version (2.91.0). Remove once
    # NixOS/nixpkgs#576874 reaches our pin.
    pythonPackagesExtensions =
      prev.pythonPackagesExtensions
      ++ [
        (pfinal: pprev: {
          fabric = pprev.fabric.overridePythonAttrs (old: {
            version = "3.2.3-unstable-2026-04-07";
            src = final.fetchFromGitHub {
              owner = "fabric";
              repo = "fabric";
              rev = "cc098ad475dbbe2e10aba72692d83aee58919570";
              hash = "sha256-Z1tGcjdLrzbqcbS6XwapSgDNpyLbBiO21yeqi7XhbpQ=";
            };
            meta = old.meta // {broken = false;};
          });
        })
      ];
  };

  # When applied, the unstable nixpkgs set (declared in the flake inputs) will
  # be accessible through 'pkgs.unstable'

  ## Switch to default unstable, YOLO
  # unstable-packages = final: _prev: {
  #   unstable = import inputs.nixpkgs-unstable {
  #     system = final.system;
  #     config.allowUnfree = true;
  #   };
  # };
}
