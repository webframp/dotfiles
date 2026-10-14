# WSLg Crash Resilience

## Goal

Stop WSLg compositor crashes from costing a working session. Two tracks: make the
editor workflow survive a weston death (fully in our control), and gather enough
evidence to either pin the WSL version or file a usable upstream bug (not in our
control).

Explicit non-goal: fixing weston itself. It runs inside the WSLg system distro
image, which we do not build or control.

## Current State

Emacs runs as `emacs-pgtk-with-packages-30.2`, a Wayland-native client of WSLg's
weston. When weston dies, the editor dies with it, and WSLg does not come back
without a full `wsl --shutdown`. Frequency has increased noticeably over recent
weeks.

### Environment as of 2026-08-09

| Component | Version / date |
| --- | --- |
| WSL Store app | 2.7.11.0 |
| `wsl.exe` | 2026-07-24 |
| `msrdc.exe` | 2026-06-04 |
| `system.vhd` (WSLg 1.0.73.2, weston `04d436c`) | 2026-05-18 |
| Kernel | 6.18.33.2-microsoft-standard-WSL2 |
| NixOS system generation at crash | 574, activated 15:40:14 |

### Crash evidence, 2026-08-09

Captured from `/mnt/wslg/stderr.log`, reproduced here because WSLg truncates its
logs on restart and every prior crash has already been erased.

```
(EE) failed to read Wayland events: Broken pipe
[16:51:28.768] WSLGd: Run:110: pid 12 terminated with signal 13    # SIGPIPE
[16:51:29.059] WSLGd: Run:110: pid 1233780 terminated with signal 11   # SIGSEGV
[16:51:29.202] WSLGd: Run:108: pid 16 exited with status 0, msrdc.exe
   ... 9 further SIGSEGV restarts through 16:51:29.732 ...
[16:51:29.732] WSLGd: Run:121: ... exited more than 10 times in 60 seconds,
               not starting it again
[17:06:29.977 .. 17:12:07.113] WSLGd: msrdc.exe exited with status 0  # 30s heartbeat, nothing listening
```

Three things this establishes:

1. **The permanence is a supervisor policy, not a deeper failure.** WSLGd retries
   ten times inside one second, with no backoff and no later retry, then stops for
   the life of the WSL session. A sub-second transport blip becomes a dead GUI
   until reboot.
2. **The first death is SIGPIPE, not SIGSEGV.** Weston wrote to a closed pipe. In
   WSLg that peer is the RDP transport to `msrdc.exe` on the Windows side, and
   `msrdc.exe` exited immediately after. The break originates at the
   Windows/transport boundary, not inside this distro.
3. **The follow-on segfaults are secondary.** Fresh weston processes fail to
   initialize against a session whose shared-memory path and hvsocket service ID
   are already poisoned.

### Hypotheses eliminated

Recorded so this is not re-investigated later.

- **NixOS store-path churn under WSLg's app-list monitor.** `weston.log` ends with
  `app_list_monitor_thread` enumerating the 14 `.desktop` symlinks that
  `wsl.startMenuLaunchers = true` installs, then freeing all of them. Ruled out:
  the last `nixos-rebuild` was 15:40:14 and the crash was 16:51:28, 71 minutes
  apart. The apparent stale path (weston logged `emacs-pgtk-30.2` while the
  symlink points at `emacs-pgtk-with-packages-30.2`) is real and benign, because
  the wrapper's own `Exec=` line references the inner store path. Those log lines
  are ordinary teardown following the crash.
- **GPU driver reset / TDR.** Windows System log holds six events across six hours,
  none from a display provider, no `LiveKernelEvent`, no 4101. The boot-time
  `dxgkio_query_adapter_info: Ioctl failed: -22` in `dmesg` is unrelated noise.
- **Kernel OOM.** No OOM kills in `dmesg`.

### Leading hypothesis

Version drift across the seam that failed. The Windows-side RDP client and host
binaries were replaced weeks after the in-VM compositor image was built, the Store
app auto-updates, and the failure is exactly at that boundary.

**Open question, only answerable from memory:** did crash frequency jump around
late July 2026, when `wsl.exe` was replaced? If yes, this is a WSL 2.7.x
regression and the log excerpt above is sufficient for an upstream report.

### Confirmed side bugs

Cosmetic, and none can produce a SIGPIPE. Fix for hygiene, not as the cure.

- `FontMonitor.cpp:280` throws `No such file or directory` every boot because
  `/usr/share/fonts` does not exist. Font monitoring is silently off.
- `/usr/local/share/applications` is missing but enumerated as app list folder 1.
- `XDG_RUNTIME_DIR /mnt/wslg/runtime-dir` is mode 0777, so dbus refuses to create
  its transient service directory.

## Design

### 1. Decouple the editor from the compositor (highest value)

Run Emacs as a daemon and attach frames to it. A weston death then costs a frame
rather than the session, and reattaching is immediate in the terminal with no WSL
restart.

Emacs arrives via the user-environment profile, so home-manager's module is
probably the right owner rather than the NixOS one. **Verify which module owns the
package before implementing.**

```nix
# home/sme/... (home-manager)
services.emacs = {
  enable = true;
  defaultEditor = true;
  # package should resolve to the existing emacs-pgtk-with-packages
};
```

Usage after that:

- `emacsclient -c` for a GUI frame under WSLg
- `emacsclient -nw` for a terminal frame, including while WSLg is dead
- buffers, undo history, and server state survive compositor restarts

A socket directory already exists at `/run/user/1000/emacs`, so a server has run
before, but no daemon is running now and no `services.emacs` appears anywhere in
the dotfiles flake.

### 2. Preserve crash evidence

WSLg truncates `/mnt/wslg/*.log` on restart, which is why the frequency claim is
currently unfalsifiable. Copy the logs aside whenever they change, so crashes
accumulate into a timestamped history.

Sketch, needs the unit type decided (a `systemd.paths` watch on `stderr.log` is
probably cleaner than a polling timer):

```nix
systemd.services.wslg-log-archive = {
  description = "Archive WSLg logs before truncation";
  serviceConfig.Type = "oneshot";
  script = ''
    dest=/var/log/wslg-history/$(date +%Y%m%d-%H%M%S)
    mkdir -p "$dest"
    cp /mnt/wslg/stderr.log /mnt/wslg/weston.log "$dest/" 2>/dev/null || true
  '';
};
```

Once a few crashes are on record, correlate timestamps against Windows Update
history and `wsl.exe` replacement dates to settle the regression question.

### 3. FHS shims for the side bugs

```nix
systemd.tmpfiles.rules = [
  "d /usr/share/fonts 0755 root root -"
  "d /usr/local/share/applications 0755 root root -"
];
```

### 4. Upstream or pin

If frequency did jump in late July, file against `microsoft/wslg` with the
stderr excerpt above, emphasizing the WSLGd give-up policy as the thing that
turns a transient fault into a session-length outage. Independently, consider
whether the WSL Store app should be held at a known-good version.

## Recovery Runbook

Until any of the above lands:

1. `emacsclient -nw`, or `emacs -nw`, to keep working in the terminal.
2. When a GUI is genuinely needed, `wsl --shutdown` from Windows, which
   terminates every WSL session including any running agent sessions.
3. Before shutting down, copy `/mnt/wslg/stderr.log` and `/mnt/wslg/weston.log`
   somewhere durable, since the restart erases them.

## Experiment: manual WSLGd relaunch via `wsl.exe --system`

`wsl.exe --system` opens a root shell in the hidden system distro where WSLGd
and weston actually run, separate from any user distro. This is undocumented,
unsupported territory, but it's the only foothold found so far short of a full
`wsl --shutdown`.

Process tree while healthy (2026-08-10):

```
root    9    1  /usr/bin/WSLGd
wslg   14    9   \_ /usr/bin/weston --backend=rdp-backend.so --modules=wslgd-notify...
wslg   25   14   |   \_ /usr/bin/Xwayland :0 -rootless ...
wslg   17    9   \_ /init .../msrdc.exe ...
```

WSLGd's parent is `/init` directly, not systemd or another service manager
with a declared restart policy, so an auto-respawn on kill can't be assumed.
The plan is to capture WSLGd's and weston's exact invocation now, while
healthy, so a manual relaunch (rather than a hopeful `kill` and wait) is
possible once a real crash puts WSLGd into its permanent give-up state.

Prep (safe, read-only, run while the session is healthy):

```
wsl.exe --system
tr '\0' ' ' < /proc/9/cmdline; echo
tr '\0' ' ' < /proc/14/cmdline; echo
cat /proc/9/environ | tr '\0' '\n'
```

Captured 2026-08-10:

```
WSLGd cmdline:   /usr/bin/WSLGd                       # no args
weston cmdline:  /usr/bin/weston --backend=rdp-backend.so --modules=wslgd-notify.so
                 --xwayland --socket=wayland-0 --shell=rdprail-shell.so
                 --log=/mnt/wslg/weston.log --logger-scopes=log,rdp-backend,rdprail-shell
```

`/proc/9/environ` was empty even under `sudo` — WSLGd takes no flags and its
env is unreadable (either scrubbed post-startup, or restricted for PID 1's
descendants even as root). WSLGd's configuration source is therefore still
unknown; weston's own invocation is fully captured above, so a manual weston
relaunch is reproducible even without WSLGd's env. Whether weston can be
started standalone (without WSLGd) and still get a working RDP-backed
Wayland session is untested — worth checking weston's own env next
(`/proc/14/environ`), and whether WSLGd instead reads a config file rather
than env vars.

Recovery attempt, next time WSLGd has actually given up (per the "not
starting it again" log line):

1. `wsl.exe --system` → root shell.
2. `ps -ef --forest` → confirm WSLGd is alive but refusing to spawn children
   (vs. actually dead).
3. Kill WSLGd, then manually re-exec it with the captured cmdline/env,
   attempting to reproduce the real invocation rather than guessing at flags.
4. Check whether a fresh weston comes up, and whether a GUI app or
   `emacsclient -c` from Windows can reach it — without `wsl --shutdown`.

Open risk: WSLGd's setup may depend on VM-level plumbing (hvsocket service
registration, shared memory paths) done earlier in boot, outside what a bare
re-exec from a root shell can replicate. If so, this experiment fails and
`wsl --shutdown` remains the fallback, same as today.

## Open Questions

- Did the crash rate increase around 2026-07-24?
- Does home-manager or the NixOS module own the emacs package?
- Is `wsl.startMenuLaunchers = true` worth keeping? It is not implicated in the
  crash, but it is the only thing feeding the app-list monitor, and disabling it
  is a cheap experiment if crashes continue after the decoupling work.
