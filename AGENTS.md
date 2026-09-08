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

- Prefer the `Makefile`; run its targets from the repo root.
  - `make` (default) / `make dry-run` — simulate the deployment.
    It writes nothing, so it is safe at any time.
  - `make switch` — deploy every package in `.stow-packages`.
  - `make check` — compare the live `$HOME` against this checkout and
    report `MISSING`, `DRIFT`, or `UNDECLARED`.
  - `make brew` / `make brew-diff` — install the declared Homebrew
    packages, or list installed packages that the Brewfile omits.
  - `make help` — list every target.
- Run `make check` after any change to `.stow-packages`, and whenever
  an app might have replaced a link with a real file.
- Edit files in this repo. Do not edit the deployed links in `$HOME`.

## Terminals

- Primary: **Alacritty** (`alacritty/.config/alacritty/alacritty.toml`).
  No window decorations, Berkeley Mono 10pt, dynamic padding, fish
  login shell (`/opt/homebrew/bin/fish -l`). Dock-pin manually.
- Fallback: **Ghostty** (`ghostty/.config/ghostty/config`). Same
  font and font-size for visual continuity. Kept for cases where
  Ghostty's split keybindings or its specific GPU compositing
  behavior is preferred.
- iTerm2 remains installed via Brewfile as a deep-legacy fallback
  but is not tracked or actively maintained.

Both tracked terminals deliberately use Berkeley Mono at size 10 so
visual state is identical across them.

## Multiplexer

- **tmux** (`tmux/.tmux.conf`) is the in-terminal multiplexer.
  Prefix is backtick (`` ` ``). Splits: `` `\ `` (vertical),
  `` `- `` (horizontal). Window navigation: `` `, `` / `` `. `` to
  cycle, `` `< `` / `` `> `` to reorder. Pane motion via
  M-h/j/k/l (Alt-h/j/k/l) is unified with Neovim splits through
  `vim-tmux-navigator` — the same chord crosses tool boundaries.
- **sesh** (`brew install sesh`) is the session orchestrator.
  `` `T `` inside tmux opens an fzf-tmux picker over tmux
  sessions, zoxide directories, git repos, and config dirs. Five
  ctrl-chord filters narrow the picker (all / tmux / configs /
  zoxide / find / kill). Sesh has no tracked config — uses
  sensible defaults.
- Plugins live at `tmux/.tmux/plugins/`; tpm is a git submodule
  at `tmux/.tmux/plugins/tpm`. Other plugins are gitignored
  (TPM installs them at runtime via `prefix + I` on first launch).

## Editors

- **Emacs** (`emacs/.emacs.d/`) is the primary heavy-edit editor.
- **Neovim** (`nvim/.config/nvim/`) is the fast in-tmux companion.
  lazy.nvim plugin manager; rose-pine theme (transparent,
  terminal-synced via OSC 11); snacks.nvim for picker / dashboard
  / toggles; neogit for in-editor git workflows. Leader is space.
  M-h/j/k/l navigates splits (same chord as tmux panes via
  vim-tmux-navigator on both sides).
- lazy.nvim auto-installs the plugin set from `lazy-lock.json`
  on first `nvim` launch (pinned commits for reproducibility).

## Agent stack

- Claude Code config: `claude/.claude/settings.json`. Permissions
  split into `allow` / `deny` / `ask` tiers; personal overrides via
  untracked `~/.claude/settings.local.json`.
  **Seed-only.** Claude Code owns the regular local settings file.
  Stow excludes the tracked template; `make switch` seeds it only if
  absent. Existing regular files are preserved byte-for-byte. Legacy
  links are backed up and detached without changing their contents.
  Template changes apply to new machines only. Migrate existing local
  preferences explicitly, never overwrite them with the template.
  See `docs/config-ownership.md` for the complete ownership model.
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
- Harness-agnostic preferences: `agents/.agents/preferences.md` is the
  single source for cross-harness policy — communication, attribution,
  git workflow, secrets, verification. Stow fans it out to
  `~/.claude/CLAUDE.md`, `~/.codex/AGENTS.md`, and
  `~/.pi/agent/AGENTS.md` through in-repo symlinks, so all three read
  the same bytes and cannot drift. Edit the canonical file, never a
  deployed link; restart agent sessions to reload. Harness-specific
  rules do not belong in it.
- Harness-agnostic skill library: `agents/.agents/skills/<name>/SKILL.md`.
  Any harness following the SKILL.md convention (Pi via its skills
  loader, Claude Code via plugins) can consume them. Skills are
  reusable across tool churn — they outlive any single harness.
- Cross-agent coordination scratch: per-directory `.context/`
  directories (gitignored repo-wide) hold ephemeral plans, notes,
  and intermediate output shared between agent invocations. Not
  for committed documentation — for short-lived runtime state
  that humans rarely review but agents need to hand off.
