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
