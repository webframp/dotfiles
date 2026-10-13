# herdr keybindings and copy/paste: follow-up

## Starter prompt (paste this to begin the next session)

> I'm replacing tmux with herdr (v0.9.3). The config is generated from
> `webframp.herdr.settings` in `modules/home-manager/herdr.nix`
> (ported from `modules/home-manager/tmux.nix` and
> `home/sme/shared/includes/tmux.conf`). `prefix+u` (URL popup) is tested and
> works. I don't care about resurrect-style session restore. Copy/paste is the
> priority. I ran the clipboard tests in the "Tests to run" section of
> `docs/plans/2026-10-09-herdr-keybindings-followup.md`; my results are below.
> Help me refine the keybindings and fix any clipboard gaps. Read that file
> first, and use the herdr docs index at https://herdr.dev/llms.txt (the raw
> pages are under `raw.githubusercontent.com/herdrdev/herdr/v0.9.3/docs/next/website/src/content/docs/`).
>
> My test results: <fill in per test: pass / fail / what happened>

## What we learned

### Model differences from tmux

- Hierarchy is workspace → tab → pane. A workspace is the per-repo or per-task
  unit. A "session" is a separate server with its own panes and state
  (`herdr session attach <name>`), not a tmux session equivalent.
- Any pane can be a plain shell. Agents (claude, codex, kiro) are recognized
  foreground processes with states `idle`/`working`/`blocked`/`done`.
  `prefix+g` opens the picker (`b` filters blocked); `prefix+o` jumps to the
  notification. This replaces `tmux-jump-bell`.
- Resume after server restart needs an agent integration. Claude and codex have
  them; kiro does not (state-only). `herdr.nix` installs only the binary, not
  the integrations. Low priority, since session restore isn't wanted.
- Scriptable: `herdr agent start|prompt|wait`, `herdr pane run|read|wait-output`,
  plus a socket API.

### Copy mode (documented)

| Action | tmux (current) | herdr |
|---|---|---|
| Enter | `prefix+[` | `prefix+[` (`keys.copy_mode`) |
| Select | `v`, `C-v` rectangle | `v` or `Space`; `V` may select lines; no rectangle found |
| Copy | `y` (tmux-yank → `xclip -selection clipboard -i`) | `y` or `Enter` |
| Search | `/` `?` | `/` `?`, `n` `N` (smart-case) |
| Exit | `q` / `Esc` | `q` / `Esc` |
| Scrollback in editor | none | `prefix+e` (`keys.edit_scrollback`) |

- Copy mode does not pause the pane. Output stays live, and selections track
  live text rather than a snapshot. Resize or alt-screen switch clears them.
- `ui.copy_on_select` defaults to true: mouse drag or double-click copies on release.
- `ui.mouse_capture` defaults to true, so WezTerm's own selection probably needs
  a modifier such as shift.
- Relevant config keys: `ui.copy_on_select`, `ui.mouse_capture`,
  `ui.mouse_scroll_lines`, `ui.toast.clipboard.enabled`,
  `ui.right_click_passthrough_modifier`, `advanced.scrollback_limit_bytes`,
  `keys.clear_pane`, `keys.remote_image_paste` (default `ctrl+v`).

### Not documented (the open questions)

- How herdr writes to the system clipboard: OSC 52 to the outer terminal, or a
  native helper?
- Whether it forwards OSC 52 from inside panes (Claude Code `/copy`). tmux needs
  `allow-passthrough on` for this; I found no herdr equivalent.
- Whether there is an internal paste buffer (tmux `prefix+]`), or whether paste
  is just WezTerm's.
- Whether it has a `prefix+y`-style "copy buffer to clipboard" outside copy mode.

### Gaps versus the tmux setup

- No `tmux-thumbs` equivalent (hint-based copy of paths, hashes, IPs).
- No `prefix+y`.
- No rectangle selection found.
- Current herdr config remaps: prefix `ctrl+j`, detach `prefix+d`, resize on
  `prefix+shift+hjkl`, swap on `prefix+alt+hjkl`. herdr's docs and `prefix+?`
  show the defaults, so they will disagree with this config.

### WSL clipboard context (from `project_wsl_terminal_stack` memory)

- xclip via the X11 WSLg bridge round-trips unicode correctly. `clip.exe` and
  the Wayland bridge (`wl-copy`) corrupt non-ASCII.
- tmux works today because of `allow-passthrough on` (OSC 52 → WezTerm →
  Windows) plus `@override_copy_command` = xclip.
- kiro-cli uses arboard/Wayland, so it is aliased with `WAYLAND_DISPLAY=`.

## Tests to run

Run each in herdr under WezTerm on galvatron-wsl, and paste into a Windows app.
Use text with unicode, e.g. `héllo → 世界`.

1. Plain shell pane: mouse drag-select, then paste in Windows.
2. Plain shell pane: `prefix+[`, `v`, move, `y`, then paste in Windows.
3. Claude pane: `/copy`, then paste in Windows (OSC 52 passthrough case).
4. Kiro pane: mouse drag-select, then paste in Windows.
5. Does WezTerm's own selection work with shift held?
6. How do you paste into a herdr pane (WezTerm's paste key, middle-click)?

## Questions to start the next session

1. Which of tests 1–6 failed, and what exactly happened (nothing copied,
   garbled unicode, wrong clipboard)?
2. If test 3 failed: does herdr have an OSC 52 passthrough or clipboard-forwarding
   setting? Check `config-reference.json`, the GitHub issues, and whether to file one.
3. Do you want `ui.copy_on_select = false` so copying is deliberate, or is
   copy-on-drag fine?
4. Should `Enter` in copy mode keep copying, or should it match tmux and only
   copy on `y`? (herdr has no obvious key to unbind.)
5. Do you want a `prefix+y` replacement, e.g. a `[[keys.command]]` that pipes a
   pane read into `xclip`? A tmux-thumbs-style hint picker is the larger option.
6. Do you want `prefix+e` (scrollback in `$EDITOR`) kept as is, or moved?
7. Do the `prefix+alt+hjkl` swap bindings feel right, or is it better to give up
   swap and keep the pain-control resize keys alone?
8. Do you want the claude and codex integrations wired into `herdr.nix`, or
   skipped since you don't need restore?

## Files

- `modules/home-manager/herdr.nix` — module; config.toml is generated from `webframp.herdr.settings`
- `home/sme/shared/includes/herdr-launch.sh` — `prefix+f` project picker and `prefix+shift+a` agent-worktree popups
- `home/sme/shared/includes/tmux.conf`, `modules/home-manager/tmux.nix` — what's
  being ported (clipboard section: lines 16–17, 38–45, 66–72)
