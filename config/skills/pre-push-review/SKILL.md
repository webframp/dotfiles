---
name: pre-push-review
description: >
  Local adversarial review loop before pushing. Shows the diff, sends it to a
  subagent reviewer with architecture context, iterates fix/gate/review until
  PASS, then stops and asks for push confirmation. Triggers on "pre-push
  review", "review before push", "local review loop", "review my changes",
  "ready to push?", "check before I push".
---

# Pre-Push Review Loop

Run a local adversarial review on uncommitted or unpushed changes. Iterate
until the review passes, then stop and ask the human for push confirmation.

## When to Use

After making changes and before pushing to a remote branch. Especially
critical for:
- Fixes to prior adversarial review findings (regression risk is high)
- Concurrent-access code (locks, sequences, distributed state)
- Any change touching error handling or fallback paths
- New code (>200 LOC) — fresh code has too much surface area for a single pass

## The Loop

```
┌─────────────────────────────────────────────────────┐
│                                                     │
│  ┌──────────┐    ┌───────────┐    ┌──────────────┐ │
│  │  SHOW    │───►│   RUN     │───►│  EVALUATE    │ │
│  │  DIFF    │    │  GATES    │    │  VERDICT     │ │
│  └──────────┘    └───────────┘    └──────┬───────┘ │
│                        ▲                 │         │
│                        │           ┌─────┴─────┐   │
│                        │           │           │   │
│                        │         PASS        FAIL  │
│                        │           │           │   │
│                        │           ▼           ▼   │
│                        │     ┌──────────┐ ┌──────┐ │
│                        │     │  ASK TO  │ │ FIX  │ │
│                        │     │  PUSH    │ │      │ │
│                        │     └──────────┘ └──┬───┘ │
│                        │                     │     │
│                        └─────────────────────┘     │
│                                                     │
└─────────────────────────────────────────────────────┘
```

## Phase 1: Show Diff

Collect the changes to review:

```bash
# Uncommitted changes against HEAD:
git diff HEAD

# Or unpushed commits against remote:
git diff origin/main...HEAD
```

Use whichever is appropriate. If there are uncommitted changes, review those.
If all changes are committed but unpushed, review the full branch diff.

## Phase 2: Run Quality Gates

Before sending to the reviewer, verify the code compiles and passes local
checks. This phase has two parts: standard gates and repo-specific gates.

### 2a. Standard Gates

Discover gates from the project's config files:

| Config file | Commands |
|-------------|----------|
| `deno.json` | `deno task check && deno task lint && deno task fmt:check && deno task test` |
| `package.json` | `npm run lint && npm run build && npm test` |
| `Cargo.toml` | `cargo check && cargo clippy && cargo test` |

If gates fail, fix them first. Do not send broken code to the reviewer.

### 2b. Repo-Specific CI Gates

Standard gates catch compile/lint/test failures. They do NOT catch structural
or convention failures that CI validates separately.

**Always check for repo-level guidance first.** Read steering files, skills,
CLAUDE.md/AGENTS.md, or contributing docs in the target repository. These
define what CI expects beyond standard compilation. Follow that guidance
exhaustively — run EVERY command listed, not a subset. Skipping a single
command from the checklist is the same as skipping Phase 2 entirely.

If no explicit guidance exists, fall back to these heuristics:

1. **Check CI workflow files** (`.github/workflows/`, `.gitlab-ci.yml`).
   Identify validation steps beyond standard gates and run them locally.

2. **Compare directory structure against a peer.** In repos with many
   packages of the same kind, your new addition must match the structure
   of existing ones (marker files, metadata files, test file conventions).

3. **Run repo-specific validation commands locally.** If CI runs a
   validation command, run it yourself first.

**The principle:** if CI will run it, you run it first. "It compiled and
the tests passed" is necessary but not sufficient.

### 2c. Swamp Extension Version and Manifest Gates

When changes touch swamp extensions (any directory with a `manifest.yaml`),
run these additional checks BEFORE the adversarial review. These fail
frequently and are easy to miss:

1. **Version alignment.** The `version` field in `manifest.yaml` MUST match
   the `version` field in the model's TypeScript source (`export const model =
   { version: "..." }`). If you bumped the model version, bump the manifest.
   Verify with `swamp extension version --manifest manifest.yaml --json`.

2. **Upgrade chain integrity.** If the model has an `upgrades` array, the last
   entry's `toVersion` MUST equal the model's `version` field. Every upgrade
   entry MUST have a unique `toVersion` in ascending order. A version bump
   without a corresponding upgrade entry means cached data from the previous
   version will not be migrated.

3. **Manifest format validation.** Run `swamp extension fmt manifest.yaml
   --check --json`. This catches YAML formatting issues that the push rejects.

4. **Quality gate.** Run `swamp extension quality manifest.yaml --json`.
   Verify `status: "passed"` and no degraded checks.

5. **Dry-run push.** Run `swamp extension push manifest.yaml --dry-run`.
   This exercises the full bundler, archive builder, and schema validation
   pipeline. If it fails here, it fails in CI.

**Common failure modes:**
- Bumping `model.version` but not `manifest.yaml` version
- Adding an upgrade handler but using the wrong `toVersion` string
- Forgetting to add `days` or `fetchedAt` to a new resource spec that
  requires it (schema validation fails on push)
- Leaving the old spec name in a `writeResource` call after renaming specs

## Phase 3: Adversarial Review via Subagent

The subagent MUST have the same capability as a CI adversarial reviewer.
A diff blob with prose context is insufficient — the reviewer must read
the actual source files to trace execution paths, verify cross-function
consistency, and find protocol-level issues.

### Subagent Setup

Use a general-purpose subagent (fresh context, read access to the repo). Provide:

1. **File paths to read** — tell the subagent which source files to read
   in full. The diff identifies *what* changed; the source files reveal
   *what interacts with* what changed. The subagent must read the complete
   implementation files, not just the diff.

2. **The diff** — for awareness of what's new vs existing code.

3. **Protocol and boundary context** — describe the external system's
   interface contract. For APIs: how input is parsed (line-delimited?
   shell-tokenized? JSON?). For CLI wrappers: how arguments are
   concatenated. For network protocols: framing, escaping, delimiters.
   The reviewer cannot find injection bugs without understanding the
   protocol boundary.

4. **Sibling consistency requirement** — if the code has multiple
   functions that do similar things (e.g., multiple command builders,
   multiple SSH invocations), explicitly instruct the reviewer to verify
   that ALL siblings apply the same validation/escaping/timeout patterns.
   Inconsistency between siblings is the #1 class of bug that single-pass
   reviews miss.

### Review Prompt Requirements

The subagent's review instructions MUST include these specific probes
(not just "assume broken"):

1. **Trace every interpolation site.** For each user-supplied value that
   ends up in a command string, network request, or file path: what
   characters can break the framing? Spaces, newlines, quotes, semicolons,
   pipes, backticks, dollar signs, null bytes — each has meaning in
   different contexts.

2. **Verify sibling consistency.** If function A validates input X, do
   ALL functions that accept the same class of input apply the same
   validation? List every sibling and confirm.

3. **Check timeout/resource bounds on every external call.** Every
   subprocess spawn, every network request, every fan-out: is there a
   hard deadline? What happens if it hangs? What happens if output is
   unbounded?

4. **Test with concrete breaking inputs.** For each finding, provide the
   exact input string that breaks it. "This could be a problem" is not a
   finding — "input X produces output Y which the server interprets as Z"
   is a finding.

5. **Verify the bootstrap/lifecycle path.** Can the code be invoked in
   the order the documentation describes? Are there circular dependencies
   between configuration and the methods that produce that configuration?

6. **Check truthiness vs nullability.** In JavaScript/TypeScript,
   `if (x)` skips `0`, `""`, `NaN`, `null`, `undefined`. For numeric
   parameters, `0` is often a valid value. For optional strings, `""`
   may be meaningful.

### Verdict Format

Require the subagent to produce:

```
### [CRITICAL|HIGH|MEDIUM|LOW] <title>
**File:** <path>
**Line(s):** <range>
**Issue:** What is wrong
**Breaking input:** The exact value that triggers the bug
**Impact:** What breaks
**Fix:** How to fix it

## VERDICT: PASS or FAIL
```

FAIL if any CRITICAL or HIGH findings exist. PASS otherwise.

## Phase 4: Evaluate Verdict

| Verdict | Action |
|---------|--------|
| PASS (no HIGH+) | Proceed to Phase 5 |
| FAIL (any HIGH+) | Fix findings, return to Phase 2 |

### On FAIL

- Fix ALL findings (HIGH, MEDIUM, and LOW). Do not defer.
- After fixing, re-run quality gates (Phase 2)
- Re-run the adversarial subagent (Phase 3) on the CURRENT source files
- The re-review MUST re-read the source files — do NOT just re-review
  the new diff. Fixes often introduce new inconsistencies.
- Do NOT push between iterations

### Minimum Iterations

- For new code (>200 LOC of implementation): run at minimum 2 review
  iterations even if the first passes. Fresh code has too much surface
  area for a single pass to catch everything.
- For fixes to existing code (<50 LOC): one passing iteration is
  sufficient.

### Iteration Cap

Maximum 5 review iterations. If still failing after 5:
- Summarize what was fixed
- List remaining findings
- Ask the human for guidance — do not push in a failing state

## Phase 5: Ask to Push

When the review passes:

1. Show a brief summary of what changed
2. State: "Local adversarial review passed. Ready to push?"
3. **Wait for explicit human confirmation**
4. Only push after the human says yes

## Rules

- NEVER push without human confirmation, even on PASS
- NEVER skip Phase 2 (quality gates) — broken code wastes review cycles
- NEVER send only a diff to the reviewer — the subagent must read the
  full source files. A diff is supplementary context, not the primary input.
- NEVER defer a finding. Fix ALL findings before the next iteration.
  Deferral is not your decision — only the human can choose to ship with
  a known issue. If you think something should be deferred, ask
  explicitly. Otherwise, fix it.
- If the same finding appears twice across iterations, try a fundamentally
  different approach rather than tweaking the same fix
- After fixing findings, the re-review must check that the fix didn't
  introduce inconsistency with sibling code. Fixing function A without
  applying the same fix to sibling functions B and C is the most common
  regression pattern.
- The subagent review has confirmation bias limitations (it reads with
  fresher eyes than you, but still runs on the same model). Expect CI to
  occasionally catch things the local loop misses — but "occasionally"
  means one finding, not six rounds of fixes. If CI finds more than one
  HIGH after your pre-push loop, the loop was run incorrectly.
