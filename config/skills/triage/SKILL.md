---
name: triage
description: >
  Triage a GitHub issue end-to-end: read the issue, create a worktree, implement
  the fix or feature, run quality gates, execute the pre-push-review loop, then
  open a PR. Optionally run hostile code analysis if the issue involves external
  contributions. Triggers on "triage", "/triage", "triage this issue",
  "work on issue", "pick up issue".
---

# Issue Triage

End-to-end workflow for triaging a GitHub issue from reading through to PR
creation. Operates in a dedicated worktree to keep the main tree clean.

## Invocation

```
/triage <github-issue-url>
```

## Procedure

### Phase 1: Understand

1. **Read the issue** via `gh issue view <number> --json title,body,labels,assignees`.
2. **Identify scope** — which extensions, files, or systems are affected.
3. **Gather context** — read the affected source files, tests, manifests, and
   `deno.json` to understand the current state.
4. **Assess complexity** — determine if this is:
   - A single-extension change (one worktree, one commit, one PR)
   - A cross-cutting change (one worktree, one commit touching many extensions)
   - A multi-PR change (separate worktrees per logical unit)

### Phase 2: Plan

5. **Create a task list** covering:
   - Worktree and branch creation
   - Version bumps (CalVer via `swamp extension version --manifest`)
   - Implementation steps
   - RELEASE_NOTES.md
   - Quality gates
   - Pre-push review
   - PR creation

6. **Classify issue type and determine next action:**

   **Bug reports and feature requests (no external code attached):**
   Proceed directly to Phase 3 (Implement). These are non-code issues — the
   implementation is yours. Use a worktree, run pre-push-review, open a PR.
   No hostile code analysis needed unless the fix touches auth/secrets/CI.

   **Issues with attached code from an external contributor:**
   First, check whether the issue references code from an external contributor
   (not the repo owner). Indicators:
   - The issue was filed by someone other than the repo owner AND includes
     code snippets, a diff, or a linked PR from a fork
   - The issue links to an external PR or branch with implementation code

   > "This issue involves code from an external contributor. I recommend running
   > a hostile code analysis review before merging. Proceed with hostile analysis?"

   Default assumption: **yes** for external contributors. The human must
   explicitly decline to skip it. If yes, add the hostile code analysis step
   after pre-push review.

   **Issues from the repo owner:**
   Proceed directly to Phase 3. Ask about hostile analysis only if the change
   touches auth, secrets, CI, or dependency pins.

### Phase 3: Implement

7. **Create worktree** on a feature branch:
   ```bash
   git worktree add .worktrees/<branch-name> -b <branch-name> origin/main
   ```

8. **Bump versions first** — `manifest.yaml`, source file `version:` field, and
   upgrade chain entry. Use `swamp extension version --manifest` to get the next
   CalVer.

9. **Implement the change** — follow the issue's specification. Match existing
   patterns in the codebase. For mechanical/repetitive changes across multiple
   extensions, use Python scripts to transform systematically, then verify each
   result.

10. **Write RELEASE_NOTES.md** — describe what changed for the user who runs
    `swamp extension pull`. Follow the release-notes format in the
    project's agent instructions file (CLAUDE.md, AGENTS.md, or steering).

11. **Update manifest description** if the change adds user-visible
    capabilities.

### Phase 4: Verify

12. **Run quality gates** for each affected extension:
    ```bash
    deno task check && deno task lint && deno task fmt && deno task test
    swamp extension quality manifest.yaml --json
    ```
    All must pass before proceeding. Fix any failures.

13. **Commit** with a Conventional Commits message scoped to the extension(s):
    ```bash
    git add <specific files>
    git commit -m "feat(scope): description

    Closes #<number>"
    ```

### Phase 5: Pre-Push Review

14. **Activate the pre-push-review skill** and execute its full loop:
    - Show the diff (`git diff origin/main...HEAD`)
    - Run quality gates (already done, but re-confirm after any fixes)
    - Send diff + architecture context to adversarial subagent reviewer
    - Evaluate verdict:
      - **PASS** → proceed to Phase 6
      - **FAIL** → fix findings, re-run gates, re-review (max 3 iterations)

15. **If hostile code analysis was requested**, run it now:
    - Activate the hostile-code-analysis skill (or use the project-level
      `skills/hostile-code-analysis.md` procedure)
    - Scan for exfiltration vectors, CI manipulation, weakened security
      properties, supply chain risks
    - Verdict must be CLEAR before proceeding

### Phase 6: Ship

16. **Push the branch:**
    ```bash
    git push -u origin <branch-name>
    ```

17. **Open the PR:**
    ```bash
    gh pr create --title "<conventional title>" --body "<structured body>"
    ```
    PR body structure:
    - Summary of what changed and why
    - Files/extensions affected
    - What was tested (gate results, test counts)
    - `Closes #<number>` or `Ref #<number>`

18. **Report completion** to the human with the PR URL.

## Rules

- ALWAYS use a worktree. Never work directly on main.
- ALWAYS bump versions in the first commit of the implementation.
- ALWAYS run pre-push-review before pushing. No exceptions.
- When pre-push-review passes, push and open the PR immediately. Do not wait
  for human confirmation to push — the PR itself is the review artifact.
  What requires human confirmation is *merging*, not opening.
- ALWAYS proceed directly to implementation for bug reports and feature requests.
- ASK about hostile code analysis only when the issue involves external
  contributor code or touches auth/secrets/CI. Do not block implementation
  waiting for an answer on routine issues.
- NEVER force-push over existing PR commits.
- NEVER auto-merge. The human merges (or comments `/lgtm`/`/shipit`) after CI
  passes and any adversarial review findings are addressed.
- If the pre-push review fails 3 times, stop and ask the human for guidance.
- For cross-cutting changes (many extensions), verify each extension
  independently before committing.
- Stage specific files. Never `git add .` or `git add -A`.
- Clean up temporary scripts (Python transform helpers, etc.) before committing.

## Scope Assessment Heuristics

| Signal | Likely scope |
|--------|-------------|
| Issue mentions one extension by name | Single-extension PR |
| Issue describes a pattern applicable to many | Cross-cutting PR |
| Issue proposes architectural change | Design discussion first |
| Issue is from external contributor | Hostile analysis required (default yes) |
| Issue includes code from a fork or external source | Hostile analysis required (default yes) |
| Issue touches auth, secrets, or CI | Hostile analysis recommended |
| Issue is from repo owner, routine fix/feature | Hostile analysis not required (default no) |

## Example

```
User: /triage https://github.com/webframp/swamp-extensions/issues/301

Agent:
1. Reads issue → "add profile global arg to aws/logs"
2. Gathers context on aws/logs extension
3. Asks: "Does this need hostile code analysis?"
4. Creates worktree, implements, verifies, reviews, ships
```
