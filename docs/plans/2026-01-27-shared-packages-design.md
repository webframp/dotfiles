# Shared Packages Design

## Goal

Single source of truth for home-manager packages across platforms, with platform-specific additions handled inline via conditionals.

## Current State

- `bluestreak.nix` has ~120 packages (macOS/standalone home-manager)
- `global/default.nix` has ~100 packages (Linux hosts via NixOS)
- Significant duplication between them
- Platform-specific packages scattered in each file

## Design

### File Structure

```
home/sme/
├── packages.nix          # Single source of truth
├── global/
│   └── default.nix       # Imports packages.nix
├── bluestreak.nix        # Imports packages.nix + host extras
├── galvatron-wsl.nix     # Inherits via global (unchanged)
```

### packages.nix Implementation

```nix
{ lib, pkgs, ... }:

with pkgs;
with pkgs.nodePackages_latest;
with pkgs.tflint-plugins;

# Common packages (all platforms)
[
  awscli2
  fzf
  git
  ripgrep
  # ... merged/deduplicated list
]

# Darwin-specific
++ lib.optionals stdenv.isDarwin [
  coreutils-prefixed
  glibtool
  mas
  pinentry_mac
  pngpaste
]

# Linux-specific
++ lib.optionals stdenv.isLinux [
  coreutils
  glibcLocales
  libtool
  pinentry-curses
  xorg.xvfb
  xvfb-run
]
```

### Host File Usage

```nix
# In bluestreak.nix or global/default.nix
home.packages = import ./packages.nix { inherit lib pkgs; }
  ++ [ /* host-specific extras */ ];
```

## Key Decisions

- **Platform vs Host**: `packages.nix` handles platform differences (darwin/linux). Host files handle host-specific extras.
- **One big list**: Packages organized alphabetically within sections, not categorized by purpose.
- **Future flexibility**: This design makes eliminating `global/default.nix` easier later.

## Migration Steps

1. Create `packages.nix` with merged/deduplicated package list
2. Update `global/default.nix` to import packages.nix
3. Update `bluestreak.nix` to import packages.nix + host extras
4. Test on both platforms
