---
name: hostile-code-analysis
description: Deep automated security audit of external PRs assuming hostile intent. Searches for malicious code, backdoors, exfiltration paths, supply chain attacks, weakened security properties, and anything that could damage the maintainer's reputation. Posts a structured findings comment on the PR. Triggers on "hostile analysis", "hostile code review", "security audit PR", "check for malicious code", "is this PR safe", "audit outside contributor", "fork PR security check".
---

# Hostile Code Analysis

Deep adversarial security audit for PRs from external contributors. Assumes the
contributor is hostile and attempts to prove the code dangerous until proven
otherwise. This is not a code quality review — it is a supply chain security
gate.

## When to Use

- PRs from forks (outside contributors)
- Force-pushed branches where content may have changed between review passes
- Any PR where the maintainer wants assurance that merged code contains no
  hidden malicious behavior

## Procedure

### 1. Gather the Full Diff

```bash
gh pr view <number> --json title,body,headRefName,headRepository,files,commits
gh pr diff <number>
```

Identify all changed files. Note the head repository — if it differs from the
base repository, this is a fork PR and warrants full hostile analysis.

### 2. Scan for Exfiltration Vectors

Search the diff for any of these patterns. A single hit in production code is a
blocking finding:

- **Network calls**: `fetch`, `XMLHttpRequest`, `WebSocket`, `Deno.connect`,
  `Deno.listen`, `net.`, `http.`, `https.`, `dns.`, `URL(`, `Request(`
- **Dynamic code execution**: `eval(`, `Function(`, `new Function`,
  `import(` (dynamic), `Deno.dlopen`, `WebAssembly`
- **Environment/secret access**: `Deno.env`, `process.env`, reading `.env`
  files, accessing token/credential/secret paths
- **Filesystem beyond stated purpose**: `Deno.readFile`, `Deno.writeFile`,
  `Deno.open`, `Deno.remove`, `Deno.mkdir`, `Deno.readDir` outside test
  fixtures
- **Subprocess spawning**: any `Deno.Command` or `child_process` usage
  beyond what the extension legitimately needs. Verify the command being
  spawned is expected.
- **Timers/scheduling**: `setTimeout`, `setInterval` in production code
  (potential delayed exfil or beacon)

```bash
gh pr diff <number> | grep -in "fetch\|XMLHttpRequest\|WebSocket\|Deno\.connect\|..."
```

### 3. Check for CI/Workflow Manipulation

- Any changes to `.github/workflows/` — flag immediately
- Changes to `manifest.yaml` beyond version bumps: new `include`,
  `additionalFiles`, modified `platforms`, added `dependencies`
- New files in unexpected locations
- Changes to `deno.json` that add new dependencies (check for typosquatting)

```bash
gh pr diff <number> | grep "^diff --git"  # List all changed files
gh pr diff <number> | grep -c "\.github"   # Any workflow changes?
```

### 4. Verify Security Properties Are Not Weakened

- Read existing tests that assert security invariants (secret not in logs,
  redaction working, access controls enforced)
- Verify those tests still assert the same or stronger properties in the new
  code
- Check that mocks faithfully model production behavior — a mock that always
  returns success masks real bugs
- Look for removed assertions, relaxed validation, broader `catch` blocks,
  or weakened type constraints

### 5. Analyze Subprocess and Command Patterns

For extensions that spawn commands:

- Verify only the documented command is spawned (e.g., `security` for a
  keychain vault, `aws` for an AWS extension)
- Check that user-controlled input is properly escaped before reaching
  command arguments or stdin
- Look for command injection through unquoted interpolation
- Verify stdin/stdout/stderr handling cannot leak secrets

### 6. Force-Push Verification

If the branch was force-pushed during review:

```bash
# Compare the diff at two points — if identical, it was a rebase
gh pr diff <number> > /tmp/pr_current.diff
diff /tmp/pr_previous.diff /tmp/pr_current.diff
```

If content changed, repeat the full analysis on the new diff. Do NOT rely on
prior review conclusions for force-pushed branches.

### 7. Supply Chain Checks

- New `npm:` or `jsr:` imports: verify package names match well-known packages
  (check for typosquatting variants)
- Version pins: all dependencies must use exact versions, not ranges
- Transitive dependency changes: if `deno.json` or import maps changed, verify
  the additions are legitimate

### 8. Hidden Logic Patterns

Look for subtle malicious patterns that pass casual review:

- Conditional execution based on environment (acts benign in CI, malicious in
  production)
- Obfuscated strings (hex-encoded, base64, char code arrays) that decode to
  URLs or commands
- Unicode homoglyph attacks (visually identical characters with different
  semantics)
- Prototype pollution through `__proto__` or `constructor.prototype`
- ReDoS patterns in regex that could be weaponized
- Logic that only triggers under specific conditions (time bombs, specific
  input patterns)

## Output Format

Post a comment on the PR using `gh pr comment <number> --body '...'`. The
comment MUST follow this exact structure:

```markdown
## Automated Hostile Code Analysis — <VERDICT>

This PR was subjected to an automated adversarial security audit designed to
detect malicious code, hidden backdoors, exfiltration paths, and behaviors that
could compromise the repository or its maintainer's reputation. The analysis
assumes hostile intent from the contributor and attempts to prove the code
dangerous until proven otherwise.

### Audit Scope

All <N> changed files were examined for:

- **Exfiltration vectors**: <specific patterns checked, tailored to what this PR touches>
- **CI/workflow manipulation**: <what was checked>
- **Weakened security properties**: <what invariants were verified>
- **Hidden command execution**: <what subprocess patterns were verified>
- **Supply chain attacks**: <dependency changes checked>

### Findings

| Category | Result |
|----------|--------|
| Network/exfiltration calls | <None / description> |
| Filesystem abuse | <None / description> |
| Dynamic code execution | <None / description> |
| Environment variable access | <None / description> |
| Workflow/CI changes | <None / description> |
| Manifest scope expansion | <None / description> |
| Weakened security assertions | <None / description of what was verified> |
| Hidden subprocess spawning | <None / description> |
| <Additional rows as relevant to the specific PR> | |

### Force-Push Verification

<Include this section ONLY if the branch was force-pushed during review.
State whether content changed or whether it was a rebase (identical diff).>

### Non-Blocking Notes

<Any MEDIUM or LOW findings from the adversarial code review that are worth
noting but do not indicate hostile intent. Omit this section if there are none.>

### Verdict

**<VERDICT> — <one-sentence summary>.**
```

## Verdicts

- **CLEAR**: No hostile, malicious, or reputation-damaging code detected. Safe
  to merge (pending normal CI and code quality review).
- **SUSPICIOUS**: Patterns found that could be benign but warrant explanation
  from the contributor before merging. List specific questions.
- **BLOCKED**: Hostile or clearly dangerous patterns detected. Do NOT merge.
  Cite specific evidence.

## Severity Rules

- A single confirmed exfiltration path (network call sending project data to an
  external endpoint) is an immediate BLOCKED verdict.
- Workflow/CI changes from a fork PR are inherently SUSPICIOUS — they could be
  benign improvements, but require line-by-line justification.
- New dependencies with unusual names are SUSPICIOUS until verified against the
  official registry.
- Weakened security tests are SUSPICIOUS if the weakening is not clearly
  justified by the behavioral change (e.g., removing an assertion that the
  secret IS in argv when the fix specifically moves it out of argv is justified).
- Force-pushed content changes after a prior CLEAR verdict require full
  re-analysis. Prior conclusions are invalidated.

## What This Is NOT

This is not a substitute for:

- The adversarial code review (logic errors, edge cases, correctness)
- CI checks (type checking, linting, formatting, test execution)
- Manual human review of the design and approach

It is a security gate specifically for supply chain and hostile-actor risks
that automated CI and code quality tools do not cover.
