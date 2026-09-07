# Global agent instructions

Shared personal preferences for every agent harness. This is the one
source: `~/tilde/agents/.agents/preferences.md`. Stow links it to
`~/.claude/CLAUDE.md`, `~/.codex/AGENTS.md`, and `~/.pi/agent/AGENTS.md`,
so all three read the same bytes. Edit it here, then restart an agent
session to load the change.

Harness-specific rules do not belong in this file.

## Communication

- Be concise by default, but give enough context to act on. Go deep only
  when the task needs it, or when I ask.
- Lead with the verdict. Put the answer first and the reasoning after.
- Report actionable findings first, ordered by severity.
- Put every essential fact in the last message of a turn. I should not
  have to scroll back through tool output to learn the outcome.
- When you teach, pair the concept with hands-on execution.
- Assume I am comfortable with terminals and with advanced programming
  concepts. Do not explain the basics unless I ask.
- Write machine-readable output to STDOUT and human-facing narration to
  STDERR.
- Ask clarifying questions before you build a plan, not after.

## Identity and attribution

- Never name an AI as commit author, contributor, or `Co-Authored-By`
  trailer.
- Never add AI-generated metadata: no "Generated with" footer and no
  robot emoji, in a commit, a pull-request title or body, or a review
  comment. Naming "Claude Code" as a product is fine.
- Sign every commit through the 1Password SSH agent. Never pass
  `--no-gpg-sign`. If signing fails, find the cause, such as a locked
  vault or a missing agent socket, and stop. Do not bypass it.

## Git workflow

- Commit only with explicit authorization, and only with a verification
  trail: lint, typecheck, test, and a read of the diff.
- Commit locally by default. A request to commit, to save, or to "get it
  on main" is not permission to publish.
- Push only when I ask. Never push to `main`, `master`, or a default
  branch, and never merge a pull request or force-push, without an
  explicit instruction.
- Split unrelated changes into separate logical commits.
- Write Conventional Commits: `type(scope): summary`. Keep the summary
  imperative, at most 72 characters, with no trailing period.
- A commit message should teach. State what was wrong, then what the
  change does about it.
- Ask which files belong in a commit when it is not obvious.
- Name a development branch `jthodge/<concrete-noun>`, under 30
  characters. Rename with `git branch -m`.
- Name a final pull-request branch `feat/`, `fix/`, or `chore/`. Never
  add a `-clean` suffix.

## Secrets

- API keys, tokens, and signing material live in 1Password. Inject them
  with `op read` or `op run`.
- Never write a plaintext credential into a tracked file or a shell
  startup file.
- A pre-commit hook scans staged content for known token patterns. Treat
  `--no-verify` as an exception that needs review, not as routine.

## Verification

- Verify a refactor by tree identity. `git diff` must be empty, or every
  difference must be intended and explained.
- Say plainly when a check did not run. Do not imply coverage you did not
  produce.
- Report a failure with its output. Do not summarize a failing run as a
  success.
