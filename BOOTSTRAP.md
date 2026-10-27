# Bootstrap Prerequisites

This document lists manual steps required before applying Nix configurations.

## Critical Prerequisites

These must be completed before running `home-manager switch` or `nixos-rebuild switch`.

### 1. SSH Keys

SSH keys must exist for keychain to manage them.

```bash
# Generate new key (if needed)
ssh-keygen -t ed25519 -C "your_email@example.com"

# Or restore from backup
cp /path/to/backup/id_ed25519* ~/.ssh/
chmod 600 ~/.ssh/id_ed25519
chmod 644 ~/.ssh/id_ed25519.pub
```

### 2. GPG Keys

GPG keyring must be restored and permissions fixed.

```bash
# Restore from backup
cp -r /path/to/backup/.gnupg ~/

# Fix permissions (required after restore)
chown -R $(whoami) ~/.gnupg/
find ~/.gnupg -type f -exec chmod 600 {} \;
find ~/.gnupg -type d -exec chmod 700 {} \;

# Verify key is available
gpg --list-secret-keys BE06ADB38C7F719D
```

### 3. Password Store

The `pass` password store must be initialized (used by git credentials and aws-vault).

```bash
# Initialize with your GPG key
pass init BE06ADB38C7F719D

# Or clone existing store
git clone git@github.com:user/password-store.git ~/.password-store
```

### 4. GitLab Package Registry

Required for installing internal npm packages (e.g., `@apps/jetpack`).
Obtain the registry URL and a personal access token with `read_package_registry` scope.

```bash
pass insert gitlab/package-registry-url
pass insert gitlab/package-registry-token
```

Then add `~/.profile.d/gitlab.sh`:

```bash
export GITLAB_PACKAGE_REGISTRY_PASSWORD="$(pass show gitlab/package-registry-token)"
export GITLAB_PACKAGE_REGISTRY_URL="$(pass show gitlab/package-registry-url)"

# Configure npm (~/.npmrc). Auth token key uses // without the protocol.
npm config set "@apps:registry=${GITLAB_PACKAGE_REGISTRY_URL}" --location=user
npm config set "${GITLAB_PACKAGE_REGISTRY_URL/https:/}:_authToken=${GITLAB_PACKAGE_REGISTRY_PASSWORD}" --location=user

# Configure bun (~/.bunfig.toml). Bun does not reliably read auth tokens
# from ~/.npmrc for global installs and requires its own config file.
cat > ~/.bunfig.toml <<EOF
[install.scopes]
"@apps" = { url = "${GITLAB_PACKAGE_REGISTRY_URL}", token = "${GITLAB_PACKAGE_REGISTRY_PASSWORD}" }
EOF
```

## Platform-Specific Setup

### macOS (bluestreak)

#### Homebrew

Homebrew is not managed by Nix. Install manually:

```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

#### Doom Emacs

Emacs is installed via Nix, but Doom configuration is manual:

```bash
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs
~/.config/emacs/bin/doom install
```

### Windows native (galvatron, outside WSL)

Not managed by Nix/home-manager at all — Windows has no equivalent mechanism.
Config parity with the WSL Doom setup is maintained via a one-directional
sync from this repo (`config/doom` is the source of truth); see `make
doom-sync` in the top-level `Makefile`.

#### LANG environment variable

Native Windows sets no locale environment variables, which leaves Doom's CLI
unable to infer a coding system and causes an interactive "Select coding
system" prompt during `doom upgrade`/`doom sync` — fatal in a non-interactive
shell. Set once, permanently, at the user level:

```powershell
[Environment]::SetEnvironmentVariable("LANG", "en_US.UTF-8", "User")
```

Restart the shell for it to take effect. `config/doom/+windows.el` covers the
in-session default coding system; this env var covers the Doom CLI itself.

#### Doom Emacs

Emacs and Doom's core are installed manually (same pattern as macOS below);
only the private config (`~/.doom.d`) is synced from this repo:

```powershell
git clone --depth 1 https://github.com/doomemacs/doomemacs $env:USERPROFILE\.emacs.d
& $env:USERPROFILE\.emacs.d\bin\doom install
```

Then from WSL: `make doom-sync && doom.exe sync` (run the second half from
Windows).

#### Known gaps vs. the WSL config

`config/doom/init.el` skips these modules on Windows (guarded by
`(featurep :system 'windows)`) because they depend on Linux/Nix-only
binaries or repo layouts that don't exist there: `vterm`, `mu4e`
(`:email`), `ansible`, `direnv`, `pass`, `terraform`, and the private
`ai-coach` module (needs a companion repo cloned at a matching path).
`keychain-environment` is similarly skipped in `config.el`. The private
`llm` module's tmux-send commands and Gemini key lookup (via `pass`) will
error if invoked on Windows — there's no tmux or pass binary to shell out
to.

### Linux/WSL (galvatron-wsl)

#### AWS Granted Browser

Configure browser for AWS SSO (WSL uses Windows Firefox):

```bash
granted browser set -b firefox -p /mnt/c/Users/sme/scoop/shims/firefox.exe
granted browser set-sso -b firefox
```

#### xterm-24bit Terminfo

The terminfo file is deployed by home-manager but requires compilation:

```bash
tic -x -o ~/.terminfo ~/.xterm-24bit.terminfo
```

## Post-Bootstrap Configuration

### gpg-agent.conf (Not Yet Managed)

Create `~/.gnupg/gpg-agent.conf` manually:

```conf
# macOS
pinentry-program ~/.nix-profile/bin/pinentry-mac
default-cache-ttl 84000
max-cache-ttl 84000

# Linux
pinentry-program ~/.nix-profile/bin/pinentry-curses
default-cache-ttl 84000
max-cache-ttl 84000
```

### Private Functions (Optional)

Host-specific private functions can be placed in:

```bash
mkdir -p ~/.profile.d
# Add scripts to ~/.profile.d/*.sh
```

## Verification

After bootstrap, verify critical components:

```bash
# SSH key loaded
ssh-add -l

# GPG key available
gpg --list-secret-keys

# Pass working
pass ls

# Git credentials configured
git config --get credential.helper
```

## Known Manual Areas

| Component | Status | Notes |
|-----------|--------|-------|
| SSH keys | Manual | Must exist before bootstrap |
| GPG keys | Manual | Restore + fix permissions |
| Password store | Manual | Initialize or clone |
| GitLab package registry | Manual | URL + token in pass; configured via ~/.profile.d/gitlab.sh |
| Homebrew | Manual | macOS only, not managed by Nix |
| Doom Emacs (macOS/Windows core) | Manual | Config synced (Windows) or not managed (macOS) |
| Doom Emacs config (Windows) | Manual sync | `make doom-sync` from WSL; `config/doom` is source of truth |
| LANG env var (Windows) | Manual | One-time `SetEnvironmentVariable`; fixes Doom CLI coding-system prompt |
| gpg-agent.conf | Managed | Now managed via home-manager |
| AWS Granted | Manual | Browser config per-host |

## Testing GPG/SSH Key Configuration

After applying the configuration, test the hybrid key management setup:

### 1. Pre-flight: Clear Existing State

```bash
# Kill any existing gpg-agent
gpgconf --kill gpg-agent

# Clear SSH agent keys (optional, for clean test)
ssh-add -D

# Remove old gpg-agent.conf if it conflicts
# (home-manager will manage this now)
rm -f ~/.gnupg/gpg-agent.conf.bak
```

### 2. Apply Configuration

```bash
# NixOS (galvatron)
sudo nixos-rebuild switch --flake .#galvatron

# Or standalone home-manager
home-manager switch --flake .#sme@<hostname>
```

### 3. Test Shell Startup (Instant Prompt)

```bash
# Open new terminal - should see instant prompt immediately
# No GPG passphrase prompts
# No warnings about console output
exec zsh -l
```

**Expected**: Instant prompt appears immediately, no errors or warnings.

### 4. Test SSH Key Loading

```bash
# Check if SSH key is available (may prompt if not cached)
ssh-add -l

# Test SSH connection
ssh -T git@github.com
```

**Expected**: SSH works. Key loads on first use if not cached.

### 5. Test GPG in Terminal

```bash
# This should prompt for passphrase via pinentry-curses/tty
echo "test" | gpg --clearsign

# After entering passphrase, key should be cached
# This should NOT prompt (within cache TTL)
echo "test2" | gpg --clearsign
```

**Expected**: First sign prompts for passphrase in terminal. Subsequent signs use cached key.

### 6. Test GPG in Emacs

```elisp
;; In Emacs, evaluate:
(setq epg-pinentry-mode 'loopback)

;; Then try signing something or decrypting a file
;; M-x epa-sign-region or open an encrypted .gpg file
```

**Expected**: Emacs prompts for passphrase in minibuffer (not external pinentry).

### 7. Test Git Signing

```bash
# Configure git to sign commits (if not already)
git config --global commit.gpgsign true

# Make a test commit
git commit --allow-empty -m "Test GPG signing"

# Verify signature
git log --show-signature -1
```

**Expected**: Commit is signed. Passphrase prompt appears if key not cached.

### Troubleshooting

**"Operation cancelled" error**:
- Check if gpg-agent is running: `gpg-agent --version && gpgconf --list-dirs agent-socket`
- Restart agent: `gpgconf --kill gpg-agent && gpg-agent --daemon`

**Pinentry not found**:
- Check pinentry path: `which pinentry-auto` (Linux) or `which pinentry-mac` (macOS)
- Verify gpg-agent.conf has correct pinentry-program path

**Emacs not using loopback**:
- Ensure `(setq epg-pinentry-mode 'loopback)` is in your Emacs config
- Check `allow-loopback-pinentry` is in gpg-agent.conf

**SSH key not loading**:
- Check keychain output: `keychain --eval id_ed25519`
- Verify key exists: `ls -la ~/.ssh/id_ed25519`
