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
