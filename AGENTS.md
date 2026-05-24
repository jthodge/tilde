# AGENTS.md

Discipline for any agent (human or otherwise) that touches this repo.

## Identity

- Every commit must be signed via the 1Password SSH agent. Never
  `--no-gpg-sign`; diagnose signing failures (locked vault, missing
  agent socket) rather than bypassing.
- Never add an AI as commit author, contributor, or
  `Co-Authored-By` trailer.

## Secrets

- API keys, tokens, and signing material live in 1Password. Inject
  via `op read` or `op run`. Never paste plaintext into `.zshrc`,
  `.zshenv`, `.bash_profile`, or any other tracked file.
- The pre-commit hook (`git/.config/git/hooks/pre-commit`) scans
  staged content for known patterns. Treat `--no-verify` as the
  exception, not the routine.

## Bootstrap

- `brew bundle install` provisions macOS dependencies.
- `stow $(cat .stow-packages)` deploys configurations. Idempotent.

## Terminals

- Primary: **Alacritty** (`alacritty/.config/alacritty/alacritty.toml`).
  No window decorations, Berkeley Mono 10pt, dynamic padding, zsh
  login shell. Dock-pin manually.
- Fallback: **Ghostty** (`ghostty/.config/ghostty/config`). Same
  font and font-size for visual continuity. Kept for cases where
  Ghostty's split keybindings or its specific GPU compositing
  behavior is preferred.
- iTerm2 remains installed via Brewfile as a deep-legacy fallback
  but is not tracked or actively maintained.

Both tracked terminals deliberately use Berkeley Mono at size 10 so
visual state is identical across them.

## Agent stack

- Claude Code config: `claude/.claude/settings.json` (tracked,
  designed). Permissions split into `allow` / `deny` / `ask` tiers;
  personal overrides via untracked `~/.claude/settings.local.json`.
- Claude Code slash commands: `claude/.claude/commands/*.md`. Each
  command's `allowed-tools` frontmatter scopes its permissions
  independently of the root settings; using the command IS the
  authorization.
- Per-directory scoped permissions: `<dir>/.claude/settings.local.json`
  narrows the agent surface for work confined to that directory
  (e.g., `scripts/.claude/settings.local.json` for the gh-API tools).
- Pi harness: `pi/.pi/agent/`. Subagents (scout/planner/worker/
  reviewer) decompose work by model tier; presets (`plan`, `implement`)
  toggle model + tool surface; extensions live in
  `pi/.pi/agent/extensions/`. Authenticated via OAuth subscriptions
  (`pi /login`), not API keys. See README for install path.
- Harness-agnostic skill library: `agents/.agents/skills/<name>/SKILL.md`.
  Any harness following the SKILL.md convention (Pi via its skills
  loader, Claude Code via plugins) can consume them. Skills are
  reusable across tool churn — they outlive any single harness.
