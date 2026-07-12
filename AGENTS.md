# Dotfiles Project Context

## Project Overview

Nix flake-based dotfiles managing home-manager configurations across multiple hosts:

| Host | Platform | Integration | Config File |
|------|----------|-------------|-------------|
| bluestreak | macOS arm64 | home-manager standalone | `home/sme/bluestreak.nix` |
| galvatron-wsl | NixOS x86_64 | home-manager as NixOS module | `home/sme/galvatron-wsl.nix` |
| ubuntu-wsl | Ubuntu x86_64 | home-manager standalone | `home/sme/ubuntu-wsl.nix` |
| generic | Linux x86_64 | home-manager standalone | `home/sme/generic.nix` |

## Architecture

### Flake Module Exports

Shared modules are exported via `outputs.homeManagerModules` in `flake.nix`. Configs import them cleanly:

```nix
imports = with outputs.homeManagerModules; [
  zsh
  bat
  delta
  direnv
  fzf
];
```

### Module Namespace Convention

All custom modules use the `webframp.*` namespace to clearly identify them as personal configuration options distinct from upstream home-manager options. For example:

```nix
webframp.zsh.enable = true;
webframp.tmux.enableOrgCapture = true;
webframp.git.credentialHelper = "!pass-git-helper $@";
```

This namespace is defined in each module via `options.webframp.<module>` and `config.webframp.<module>`.

### Current Shared Modules (`modules/home-manager/`)

| Module | Purpose | Key Options |
|--------|---------|-------------|
| `zsh.nix` | Shell config with p10k, completions, optimizations | `enableVterm`, `extraZplugPlugins`, `extraShellAliases`, `extraEnvVars` |
| `bat.nix` | Syntax-highlighted file viewing + batman | `enable` only (also sets `man = "batman"` alias) |
| `delta.nix` | Syntax-highlighted git diffs | `enable` only |
| `direnv.nix` | Direnv with nix-direnv | `enable`, `whitelist` (directory prefixes) |
| `fzf.nix` | Fuzzy finding with bat/eza previews | `enable` only |
| `git.nix` | Git config with aliases, signing, user identity | `enable`, `credentialHelper` (optional) |
| `tmux.nix` | Tmux with dracula theme, resurrect/continuum | `enable`, `enableOrgCapture` (macOS), `terminal` (WSL/NixOS) |

### Per-Host Configuration (NOT extractable)

These differ per host by necessity:
- SSH key IDs (keychain)
- GPG key IDs and signing configuration
- Git signing key
- Platform-specific packages (coreutils vs coreutils-prefixed)
- macOS-specific: emacs native compilation, pinentry_mac, podman

### Legacy: `home/sme/global/`

Originally created as a single place to define shared packages across Linux hosts (ubuntu-wsl, generic) to avoid duplicating package lists. It grew organically to include more configuration. As shared modules are extracted, global may become unnecessary and can be removed once it provides no unique value.

## Zsh Startup Optimization

Achieved ~0.47s cold start (down from 2.62s) via:
- Powerlevel10k instant prompt with `mkOrder 100`
- Daily compinit caching (24h)
- Background zcompdump compilation on login

Content ordering uses `lib.mkOrder` per home-manager PR #6479.

## Future Refactoring Ideas

Potential areas to extract or consolidate:

| Area | Complexity | Notes |
|------|------------|-------|
| Package deduplication | Medium | ~80% overlap between bluestreak and global packages. Could create shared package sets. |
| GPG module | Low | `programs.gpg` config in bluestreak. Same key used everywhere (manual setup). |
| home-manager autoExpire | Low | Only in bluestreak, useful for all hosts. |
| jqp | Low | Simple doom-one theme config, only in bluestreak. |
| Kubernetes tools | Low | k9s, kubeswitch in bluestreak. May not want on all hosts. |

**Not worth extracting:**
- Trivial enables (eza, granted, zoxide) - just `enable = true`
- keychain - different keys per host
- emacs - macOS native compilation differs from Linux

**Linux-specific (keep in global or per-host):**
- `gtk.enable`, icons, xterm-24bit.terminfo, home.language

**Global elimination:**
Once enough is extracted to modules, global may have no remaining purpose and can be removed. Linux-specific bits would move to ubuntu-wsl.nix and generic.nix directly.

## Useful Commands

```bash
make switch          # Apply config (auto-detects hostname)
make build           # Dry-run build
make diff            # Show what will change
make bump PKG=swamp  # Bump a package to latest and converge the host
make zsh-bench       # Measure zsh startup time
make clean-generations AGE=14d  # Clean old generations
```

## Conventions & Gotchas

### Updating pinned packages

Run `make bump PKG=<name>` to update a prebuilt-binary package and converge in one step. It runs `scripts/update-<name>.sh`, and when the pin actually changes it stages `pkgs/<name>` and runs the host-appropriate switch (`nixos-switch` on hosts in `VALID_NIXOS_HOSTS`, `switch` otherwise). It skips the rebuild when already current, and it stages without committing.

- `VERSION=x.y.z` pins a specific release instead of latest.
- `CLEAN=<age>` runs `clean-generations` + `clean` afterward (e.g. `make bump PKG=swamp CLEAN=5d`).

Adding a new prebuilt-binary package means adding a matching `scripts/update-<name>.sh`. The `swamp`/`coder`/`kiro` scripts are the pattern: they fetch release-asset hashes per platform and edit portably. The generic `pkg-bump` (`scripts/bump-pkg.sh`) is a different tool — it assumes a `fetchFromGitHub` source build and uses BSD `sed`, so it is macOS-only and does not apply to these binary packages.

### Doom `+local.el` is per-machine

`~/.config/doom` is a whole-directory out-of-store symlink to `config/doom/` (see `modules/home-manager/doom.nix`), so any file in `config/doom/` surfaces there — including git-ignored ones. `+local.el` holds sensitive values, so it is git-ignored and never committed. Each host needs its own copy placed at `config/doom/+local.el` (canonical source: `~/src/sescriva/doom.d/<host>/+local.el`).

If it is missing, `(load! "+local")` fails at startup. That is intentional — fail loud rather than silently drop settings — so the fix is to place the file, not to make the load tolerant.

## Refactoring Configs with Special Characters

This codebase contains configs with Unicode characters that can be easily lost during refactoring (Powerline glyphs in tmux/zsh, emoji, etc.). When refactoring such configs:

1. **Edit tool failures are a signal**: If the Edit tool can't match a string, investigate why with `cat -v` or `xxd` - special characters may be involved
2. **Verify character preservation**: After refactoring, diff the generated output against the original to catch dropped content
3. **Build success ≠ runtime success**: A passing `nix build --dry-run` only validates syntax. Always test the actual application (tmux, zsh, etc.) after config changes
4. **Use semantic tools first**: Prefer tree-sitter/LSP for understanding code structure rather than fighting with string-based edits on files with special characters
