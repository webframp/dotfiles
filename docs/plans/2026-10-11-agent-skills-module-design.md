# Agent skills module: design and plan

## Starter prompt (paste this to begin the next session)

> I want one version-tracked copy of each of my personal agent skills, linked
> into every harness I use (pi via `~/.agents/skills`, Claude Code via
> `~/.claude/skills`, Kiro via `~/.kiro/skills`). Read
> `docs/plans/2026-10-11-agent-skills-module-design.md` first, then implement
> Phase 1 only: the `webframp.agentSkills` module plus migrating
> `writing-editorial`. Stop after Phase 1 verification and show me the results
> before touching any other skill.

## Goal

Personal skills live once, in this repo, under git. Home-manager links each one
into every harness's skill directory, so the content every harness reads is
byte-identical by construction and every change (including edits made by
agents) shows up in `git diff`.

Skills installed and upgraded by other tools (swamp) stay out of scope and
must never be touched by this module.

## Why

- **Drift already happened.** Four skills exist as separate copies in both
  `~/.claude/skills` and `~/.kiro/skills`, and all four differ (details in
  "Current state"). Nothing records which copy is newer or why.
- **Agents edit skills.** Skills are instructions that future agent sessions
  obey. An edit by any session or tool silently changes behavior. Git makes
  every change reviewable and revertible; this is also a supply-chain concern
  (see the `agentic-security` skill).
- **Multiple hosts.** This flake configures bluestreak, galvatron-wsl,
  ubuntu-wsl, and generic. Skills should follow the user like the rest of the
  config.
- **Regressions.** When a skill rewrite makes output worse, `git log -p` on
  its `SKILL.md` shows exactly what changed.

## Current state (as of 2026-10-11, on galvatron-wsl)

### Skill directories

| Path | Read by | Managed by today |
|---|---|---|
| `~/.agents/skills/` | pi (native default), other Agent Skills harnesses | swamp (`swamp`, `swamp-getting-started`) + one manual skill |
| `~/.claude/skills/` | Claude Code | manual copies; swamp copies; Claude-managed `synced/` and `.trash/` |
| `~/.kiro/skills/` | Kiro CLI | manual copies; swamp copies |

None of these directories is a git repo or managed by home-manager.

### writing-editorial (the Phase 1 skill)

Rewritten in the previous session (from Klinkenborg's *Several Short Sentences
About Writing*). Current layout, set up by hand:

```
~/.agents/skills/writing-editorial/SKILL.md      real directory (canonical today)
~/.claude/skills/writing-editorial -> ~/.agents/skills/writing-editorial
~/.kiro/skills/writing-editorial   -> ~/.agents/skills/writing-editorial
```

Verified working through those single-hop symlinks in all three harnesses
(see "Verification" for the commands). The pre-rewrite version was backed up
to a session scratchpad that will not survive; git history starts with the
rewritten version.

### Drifted skills (exist in both Claude and Kiro, contents differ)

| Skill | Claude copy | Kiro copy | Nature of the difference |
|---|---|---|---|
| `hostile-code-analysis` | 2026-08-01, 8523 B | 2026-07-30, 8505 B | Frontmatter only: Claude copy has single-line description plus `metadata.version: 2026-07-30`; Kiro copy has folded description |
| `triage` | 2026-08-02 | 2026-07-30 | Harness wording: Claude says "project's CLAUDE.md (Release Notes section)" and "activate the hostile-code-analysis skill"; Kiro says "extension-conventions steering" and offers a project-level `skills/hostile-code-analysis.md` fallback |
| `pre-push-review` | 2026-08-10, 10752 B | 2026-08-13, 12455 B | Kiro copy is newer and adds a whole section, "2c. Swamp Extension Version and Manifest Gates" (manifest/model version alignment, upgrade chain, `swamp extension fmt/quality/push --dry-run`) that the Claude copy lacks |
| `loadgen-fleet` | 2026-08-02, 28673 B | 2026-07-31, 12426 B | Substantially rewritten; Claude copy is more than twice the size, with a different description |

Run `diff -r ~/.claude/skills/<name> ~/.kiro/skills/<name>` to see each in full.

### Single-harness skills (candidates for later promotion)

- Claude only: `agentic-security`, `bench`, `extension-compliance-audit`,
  `infrastructure-philosophy`, `systematic-debugging`, `using-git-worktrees`,
  `writing-plans`.
- Kiro only: `adversarial-review`, `bench-harness`, `design-review`,
  `exe.dev`, `extension-development`, `jetpack`, `pr-fix-loop`,
  `release-review`, `search-tech-docs`, `swamp-code-review`.
- Already identical in both: `doc-writing` (plus the swamp-managed pair).

Notes: `bench` (Claude) and `bench-harness` (Kiro) may be the same skill under
two names. The Kiro directory `exe.dev` declares `name: using-exe-dev`, which
violates the Agent Skills rule that `name` matches the directory name and
contains only lowercase letters, digits, and hyphens.

## Design

### Source location: `config/skills/<name>/`

Matches the existing `config/claude/`, `config/pi/`, `config/doom/` layout.
Each skill is a directory with `SKILL.md` and optional `references/`,
`scripts/`, `assets/`.

### Module: `modules/home-manager/agent-skills.nix`

Follow the repo conventions in `AGENTS.md`: two `# ABOUTME:` header lines,
`webframp.*` namespace, export from `modules/home-manager/default.nix`.

Proposed options:

```nix
options.webframp.agentSkills = {
  enable = lib.mkEnableOption "personal agent skills linked into every harness";

  skills = lib.mkOption {
    type = lib.types.listOf lib.types.str;
    default = [];
    description = "Skill directory names under config/skills to link.";
  };

  targets = lib.mkOption {
    type = lib.types.listOf (lib.types.enum ["agents" "claude" "kiro"]);
    default = ["agents" "claude" "kiro"];
    description = "Harness skill directories to link into.";
  };
};
```

Target paths: `agents` = `~/.agents/skills`, `claude` = `~/.claude/skills`,
`kiro` = `~/.kiro/skills`.

Enable in `home/sme/shared/base.nix` next to `webframp.claudeCode.enable`,
with `skills = ["writing-editorial"];`. List options merge, so a host can add
skills without repeating the shared list.

### Decision: explicit list, not auto-discovery

`builtins.readDir ./config/skills` would make adding a skill zero-config, but
it hides what each host gets and makes per-host subsets awkward. The explicit
list costs one line per skill and reads clearly. Either way, flake evaluation
only sees git-tracked files, so a new skill must be `git add`ed before
`make switch` (this is a common surprise; document it in the module comment).

Add an assertion that every listed name has `config/skills/<name>/SKILL.md` in
the flake source, so a typo fails at build time instead of producing a
dangling link.

### Decision: direct activation symlinks, not `home.file` + `mkOutOfStoreSymlink`

`modules/home-manager/claude-code.nix` documents why: `home.file` with
`mkOutOfStoreSymlink` produces a double-hop symlink (target -> home-files store
profile -> dotfiles). Claude Code's atomic writer (write `.tmp`, then rename)
resolves only one hop, lands in the read-only store, and fails with `EROFS`.
Agents edit skills with exactly that kind of writer, so skills need the same
treatment: one `ln -s` from the harness directory straight to the working copy.

It is not verified whether the double hop breaks directory symlinks the way it
breaks `settings.json`. Use the direct pattern anyway, for consistency and
because the failure mode only appears when an agent tries to edit a skill.

Single-hop symlinks to a directory are verified to work in all three harnesses
(that is the current `writing-editorial` setup). Pi deduplicates skills by
canonical path, so the same skill reached through several links will not
produce name-collision warnings.

### Activation script requirements

Run with `lib.hm.dag.entryAfter ["writeBoundary"]`, like `linkClaudeSettings`.
For each target directory and each skill:

1. `mkdir -p` the target directory.
2. If `<target>/<name>` is a symlink, replace it with `ln -sfn`.
3. If `<target>/<name>` is a **real directory or file**, do not touch it. Print
   a warning naming the path and skip that link. Do not fail the whole switch
   for one skill, and never delete user content.
   - **Gotcha:** `ln -sfn src dest` when `dest` is an existing real directory
     creates `dest/<name>` *inside* it instead of replacing it. Check with
     `[ -L ]` / `[ -e ]` before linking.
4. If nothing is there, create the link.

Cleanup of removed skills: write the list of links created to a state file
(e.g. `~/.local/state/webframp-agent-skills/links`). On the next activation,
remove any previously recorded link that is no longer wanted, **only if** it
is still a symlink whose target is inside `config/skills/`. This guarantees
the module never removes anything it did not create.

Use `run` for mutating commands so `home-manager switch --dry-run` stays
honest (as `claude-code.nix` does).

### Never touch

- `swamp`, `swamp-getting-started`, or any other swamp-installed skill. Swamp
  writes identical copies into all three directories and upgrades them itself.
- `~/.claude/skills/synced/` and `~/.claude/skills/.trash/` (managed by Claude).
- Any entry not in `webframp.agentSkills.skills`.

### Content rules (one copy must serve every harness)

- Frontmatter uses only Agent Skills spec fields: `name` (must equal the
  directory name; lowercase letters, digits, single hyphens; at most 64
  characters), `description` (at most 1024 characters), and optionally
  `license`, `compatibility`, `metadata`, `allowed-tools`. Avoid
  harness-specific fields.
- Write instructions in harness-neutral terms ("the project's agent
  instructions file" rather than "CLAUDE.md" or "steering").
- When a skill truly needs harness-specific steps, put them in
  `references/<harness>.md` and tell the reader in `SKILL.md` when to load
  each one. Never fork the skill directory per harness; forking is what caused
  the current drift.

## Phases

### Phase 1: module plus writing-editorial

1. Do not include unrelated work in the commit. At planning time the working
   tree already had uncommitted changes in `config/claude/settings.json`,
   `flake.lock`, `flake.nix`, `home/sme/shared/base.nix`,
   `home/sme/shared/includes/herdr.toml`, `pkgs/swamp/default.nix`, plus
   untracked `.swamp/`, `BOOTSTRAP.md`, and two other plans. Check
   `git status` and ask before touching any of them. `base.nix` has
   pre-existing edits, so stage only your hunk.
2. Create `config/skills/` and move the real directory into it:
   `mv ~/.agents/skills/writing-editorial config/skills/writing-editorial`.
3. Remove the two manual symlinks (`~/.claude/skills/writing-editorial`,
   `~/.kiro/skills/writing-editorial`). Confirm they are symlinks with
   `ls -la` first.
4. Write the module, export it in `modules/home-manager/default.nix`, enable
   it in `base.nix`.
5. `git add` the new files (flake eval ignores untracked files), then run
   `make fmt` and `make check`.
6. Build, diff, and apply. galvatron-wsl is the only entry in
   `VALID_NIXOS_HOSTS` (home-manager runs as a NixOS module there), so use
   `make nixos-build`, `make nixos-diff`, `make nixos-switch`. Other hosts use
   `make build`, `make diff`, `make switch`.
7. Run every check in "Verification". Report results and stop.

### Phase 2: reconcile the four drifted skills

One skill at a time, with the user deciding. For each: show the full diff,
propose a merged harness-neutral version (for example, `pre-push-review`
should keep Kiro's section 2c; `triage` needs neutral wording for the
instructions-file and steering references), get approval, then move it into
`config/skills/`, move both old copies out of the way, add it to the list, and
switch. `loadgen-fleet` is the largest rewrite; expect a real decision there,
not a mechanical merge.

### Phase 3: promote single-harness skills

Ask which of the single-harness skills should become shared. Resolve the
`bench` / `bench-harness` question and fix the `exe.dev` name/directory
mismatch while promoting.

### Phase 4 (optional): validation target

A `make skills-check` that checks every `config/skills/*/SKILL.md` for: `name`
equal to the directory, the name character rules, description length of at
most 1024, and only spec frontmatter fields. Pi's loader can serve as the
validator (see below); a small script would avoid depending on pi's internals.

## Verification

Run after every switch that changes skills.

```bash
# 1. Every harness path resolves to the same file in the working copy
for d in ~/.agents ~/.claude ~/.kiro; do
  readlink -f "$d/skills/writing-editorial/SKILL.md"
done   # all three should print .../dotfiles/config/skills/writing-editorial/SKILL.md

# 2. Exactly one symlink hop (not a store path in between)
for d in ~/.agents ~/.claude ~/.kiro; do readlink "$d/skills/writing-editorial"; done

# 3. Kiro lists it
WAYLAND_DISPLAY= kiro-cli chat --no-interactive \
  "Without using tools, list only the names of the skills available to you, one per line."

# 4. Pi parses it without diagnostics. `pi -p` fails on this host with
#    "Could not load credentials from any providers", so call pi's loader
#    directly. Store paths change with each pi upgrade; find them via
#    `readlink -f ~/.nix-profile/bin/pi` and the wrapper scripts it execs.
NODE=<pi's node, e.g. /nix/store/...-nodejs-24.x/bin/node>
PI=<pi package>/lib/node_modules/pi-monorepo
"$NODE" --input-type=module -e "
const m = await import('$PI/dist/core/skills.js');
const r = m.loadSkills({ cwd: '/tmp', includeDefaults: false,
  skillPaths: [process.env.HOME + '/.agents/skills'] });
console.log(r.skills.map(s => s.name), JSON.stringify(r.diagnostics));"
```

5. Claude Code: start a fresh session and confirm `writing-editorial` appears in
   the available-skills list with the new description (it begins "Editorial
   principles for prose, drawing on Benjamin Dreyer").
6. Edit test: in a Claude Code session, have it make a trivial edit to the
   skill through `~/.claude/skills/writing-editorial/SKILL.md`; confirm the
   change appears in `git diff` in this repo with no `EROFS` error, then
   revert it.
7. Confirm swamp's skills are untouched: `ls -la ~/.{agents,claude,kiro}/skills/swamp`
   should still be real directories.

Note: `loadSkills({ includeDefaults: true })` only scans `~/.pi/agent/skills`
and `.pi/skills`; pi adds `~/.agents/skills` through its package manager
(`dist/core/package-manager.js`). An empty result from `includeDefaults: true`
does not mean discovery is broken.

## References

### This repo

- `AGENTS.md`: host table, `webframp.*` namespace, ABOUTME headers, `make`
  targets, the "fail loud" convention (Doom `+local.el`).
- `modules/home-manager/claude-code.nix`: the double-hop `EROFS` rationale and
  the direct activation-symlink pattern to copy.
- `modules/home-manager/pi.nix`, `modules/home-manager/doom.nix`: the
  `mkOutOfStoreSymlink` pattern (fine for files pi and Doom only read).
- `modules/home-manager/default.nix`: module export list.
- `home/sme/shared/base.nix`: where shared `webframp.*` modules are enabled.
- `docs/plans/2026-10-09-herdr-keybindings-followup.md`: format of a previous
  hand-off plan.

### External

- Agent Skills specification: https://agentskills.io/specification
- Pi skills documentation, shipped with the package (v0.87.1 at planning time):
  `<pi package>/lib/node_modules/pi-monorepo/docs/skills.md`. Key points: pi
  implements the spec; it discovers `~/.agents/skills` and project
  `.agents/skills`; it recurses into directories until it finds `SKILL.md`; it
  does not enforce name equal to directory, but other harnesses may.
- Pi `CHANGELOG.md` in the same package: symlinked skill directories are
  followed and deduplicated by canonical path (entries for #304, #3417, #3818).
- Claude Code skills documentation: https://code.claude.com/docs/en/skills
- Kiro CLI: reads `~/.kiro/skills/<name>/SKILL.md` and follows directory
  symlinks (verified on 2026-10-10 with the `kiro-cli chat --no-interactive`
  check above).

### Related user skills

- `writing-editorial`: the first skill to migrate.
- `agentic-security`: background on skills as a supply-chain surface.
