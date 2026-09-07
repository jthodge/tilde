---
description: High-rigor sequential PR-stack merge ceremony — reconcile, review, fix-at-first-owner, adversarially verify, then merge an arbitrary range of PRs one at a time.
argument-hint: <repo> <pr-range> [--onto <branch>] [--auto] [--contract <ref>]
allowed-tools: Bash(git*), Bash(gh*), Bash(pnpm*), Bash(npm*), Bash(node*), Bash(mkdir*), Bash(bash*), Read, Grep, Glob, Edit, Write, Agent
---

# PR-stack merge ceremony

Run the same high-rigor, **one-PR-at-a-time** review → reconcile → fix → adversarially-verify → merge process we use for stacked / contract-drifted PR ranges, over the PRs named in `$ARGUMENTS`. Operate verdict-first; assume the work is wrong until a verification trail proves otherwise.

## 0 · Parse the invocation

`$ARGUMENTS` = `<repo> <pr-range> [flags]`. Extract:

- **repo** — e.g. `campus-ai/prim` (omit → infer from the cwd's `gh` remote, `gh repo view --json nameWithOwner`).
- **pr-range** — `53-61`, `53,55,58`, or `53-61 except 59`. Resolve to concrete PR numbers and order them by **dependency** (a PR's base branch is processed before the PR). `gh pr view N --json baseRefName,headRefName` builds the chain.
- **--onto `<branch>`** — the merge target (default `main`).
- **--auto** — autonomous mode: an **adversarial-review CONFIRM replaces the manual approval gate**. Default is approval-gated (present each PR; merge ONLY on explicit human APPROVE). **Running this under `/goal` implies `--auto`** — the goal's Stop-hook is what drives "proceed to the next PR," and the adversarial fleet is the approval authority.
- **--contract `<ref>`** — the authoritative contract the PRs must conform to: a path, a merged PR/range, or a spec file (default: **the merge target branch itself is ground truth** — its current code is the contract downstream PRs were likely authored against an older version of).

If repo or range is genuinely ambiguous, ask once. Otherwise proceed. Set up a reviews dir: `<repo-root>/.context/pr-reviews/` (gitignored cross-agent state); write one `pr-NN-<slug>.md` per PR.

## 1 · Phase 0 — Ground truth (once, up front)

Before touching any PR, build an **authoritative contract sheet** from `--contract` (or the merge target). For a code stack this is the real wire/API/type surface the PRs must match; for a spec-gated stack it's the spec's enforceable clauses. Fan out reader agents if the surface is large (one per subsystem) and keep the conclusion, not the file dumps. Also map: which PRs are **stacked** (base = another PR's branch → rebase-cascade) vs independent, and flag any PR whose content looks **superseded** by work already on the target.

## 2 · Per-PR cadence (the loop — repeat for each PR, in dependency order)

For PR **N**, in a dedicated worktree (city-named under `…/workspaces/<project>/<city>`; create + `pnpm install` if fresh):

1. **Reconcile onto the current target.**
   - Single-commit PR → `git rebase --onto <target> <oldparenttip> <branch>`.
   - Multi-commit PR → **squash-then-rebase**: `git reset --soft <base>; git commit -m wip; git rebase --onto <target> <base> <branch>` (resolve conflicts ONCE, not per-commit).
   - Resolving a conflicted file: **do NOT trust `git checkout --ours/--theirs`** — they are inverted/unreliable mid-rebase. Take the target's version explicitly with `git show <target>:path > path`, then re-apply only this PR's net-new delta on top.
   - If the PR's substance already landed on the target (fix-at-first-owner absorbed it) → rebasing yields an (near-)empty diff → **CLOSE it as superseded** with a comment documenting where each piece landed. Do not force-merge an empty PR.

2. **Skeptical review fleet.** Review as a staff engineer auditing a VERY JUNIOR engineer: cynical default, assume the architecture / data-modeling / wire-contracts are WRONG until proven, scrutinize at the system-design level first. Score structured findings: `wireDrift` (vs the contract sheet), `rebaseRisk` (what conflicts/inherits), `otherDefects`, `scrubTokens`, and a `verdict`. Scale the fleet to the PR (a few finders for small, a diverse panel for large/load-bearing).

3. **Fix every finding** — all blockers, majors, AND minors, plus any explicit changes-requested. Apply **fix-at-first-owner + inherit**: introduce each corrected contract at the FIRST PR owning its type; downstream inherits on rebase. Net-negative / surgical-minimalist; root-cause, not symptom.

4. **Verification trail (green or it didn't happen):** `format → lint → typecheck → test → build` (adapt to the repo: e.g. `pnpm format/lint/typecheck/test/build`; add a codegen-drift gate if the repo has one). Then a **per-file PCRE scrub** (`git grep -nP` in a loop — a multiline shell-var pathspec under-reports, and default git-grep ERE ignores `\b`) for forbidden release tokens (milestone tags `M[1-9]`, plan paths, `image #N`, internal finding refs, codenames, `TBD`, `V1/V2`, and any AI-authorship / Co-Authored / "Generated with" / 🤖 marker).

5. **Adversarial-review gate.** Spawn an INDEPENDENT skeptic panel (perspective-diverse lenses — e.g. contract-conformance, regression/cold-path, security/AX, scrub) to confirm: every finding resolved, no regression or reintroduced-class, no NEW drift, scrub clean, trail green. A finding the gate raises is fixed and re-confirmed (bounded ~4 attempts before escalating to the human). This panel is the verification of MY redo, not the author's original — it has caught a real defect nearly every PR; never skip it.

6. **Tree-identity squash → one clean commit.** Capture the tree (`git add -A && git write-tree`), `git reset --mixed <target>`, re-stage, commit with a **pedagogical, self-contained** message (a lesson, not a diff/journey/milestone log), and assert `HEAD^{tree}` equals the captured tree byte-for-byte. Squash-merge convention: one commit per PR, `feat(...)`/`fix(...)` subject. Scrub the PR **title + body** too. (Local commits may need `--no-gpg-sign` if the signing agent fails in-env; the GitHub squash-merge re-signs the canonical commit.)

7. **CI.** `git push --force-with-lease origin <branch>:<branch>`. Then **retarget the base**: `gh pr edit N --base <target>`, and **`gh pr close N; gh pr reopen N`** to fire a fresh `pull_request` event (stacked PRs whose base was a parent branch won't trigger CI otherwise). Wait for green (poll in the background).

8. **Merge gate.**
   - **Default (approval-gated):** present a verdict-first summary (what landed, what the fleet found + fixed) and merge **only on explicit human APPROVE**.
   - **`--auto` / under `/goal`:** the adversarial-review CONFIRM (step 5) + green CI IS the approval — merge.
   - Merge: `gh pr merge N --squash --subject "<clean subject>" --body-file <clean body>`.

9. **Advance.** Rebase the next PR onto the NEW target sha and repeat. Update the per-PR review file; at the end, refresh the ceremony memory.

## 3 · Identity constraints (binding — never violate)

- **Never** add AI as author/contributor; no `Co-Authored-By`, "Generated with", or 🤖, in commits, PR bodies, or comments. ("Claude Code" as a *product* reference is allowed.)
- Never commit without explicit authorization + a Verification Trail (lint, typecheck, test, diff).
- Squash-merge, one commit per PR. Tree identity (byte-for-byte `git diff`) is the ground truth that a narrative reconciliation is functionally correct.
- Final PR branches use `feat/` `fix/` `chore/` prefixes; never a `-clean` suffix.
- AX: STDOUT machine-readable, STDERR human; verdict-first comms.

## 4 · Quality patterns (compose as the PR warrants)

- **Adversarial / perspective-diverse verify** — N skeptics, each a distinct lens, each prompted to REFUTE; kill a finding only on majority-survive.
- **Fix-at-first-owner + inherit** — correct a drift class once, at its owner; downstream inherits.
- **Loop-until-dry / completeness critic** — for open-ended hunts, keep finding until K dry rounds; a final "what's missing?" pass.
- **Surface superseded work** — closing a fully-absorbed PR is a valid disposition; document where its value landed.
- **No silent caps** — if you bound coverage, say so.

## 5 · Gotchas (learned the hard way)

- rebase `--ours`/`--theirs` inversion → use explicit `git show <target>:path`.
- multiline `$VAR` pathspec breaks scrub greps → loop per-file; use PCRE for `\b`.
- fresh worktrees need `pnpm install` (node_modules not shared).
- stacked PRs won't fire CI until base→target + close/reopen.
- a watermark/cron pipeline (classify/link/etc.) is async — poll, don't assume instant.

## 6 · Output

A merged (or closed-superseded) PR per range entry, each: one clean squash commit, green CI, a `pr-NN-*.md` review file, and a refreshed ceremony-memory entry. Close with a verdict-first roll-up: per-PR disposition, what each fleet caught, and any deferred operational debt.
