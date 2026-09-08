# Shell startup

Fish is the primary interactive shell. Alacritty selects `fish -l`;
Ghostty uses the account shell. Zsh and Bash retain legacy configuration
for direct use. This change does not alter the macOS account shell.

Both shells share three rules:

1. **No secret fetches on startup.** `op read` is slow, prompts on a
   locked 1Password vault, and leaks the plaintext to every child
   process. Credentials load lazily inside the wrapper function that
   needs them.
2. **No installations on startup.** Neither shell runs `uv venv`,
   `brew install`, or any other provisioning command as a side effect
   of opening a terminal. Missing dependencies surface when the user
   invokes the tool, not when they open a shell.
3. **No subprocess brew shellenv on non-interactive startup.** Fish
   adds `/opt/homebrew/{bin,sbin}` to `PATH` directly with
   `fish_add_path`, preserving anything the caller already exported.

## Fish phase order

`fish/.config/fish/config.fish` runs, in order:

1. **Environment (always).** Prepend Homebrew to `PATH` on macOS if
   `/opt/homebrew/bin` exists; then source `exports.fish` for
   identity, Volta, uv, Go, and Honcho peer identity.
2. **Local overrides (always).** If `$__fish_config_dir/local.fish`
   exists, source it. This runs *before* activation so a machine can
   override `UV_DEFAULT_VENV`, set `TILDE_AUTO_VENV=0`, or otherwise
   retune the phases below. `local.fish` is gitignored.
3. **Interactive-only.** `status is-interactive` gates
   `aliases.fish`, `colors.fish`, cargo env, and the tool
   integrations (`mise`, `zoxide`, `fzf`, `uv` completions, `ngrok`).
   Scripts and `fish -c '...'` skip all of it.
4. **Optional interactive base venv activation.** `__uv_autovenv` sources
   `$HOME/.venv/$UV_DEFAULT_VENV/bin/activate.fish` **if it already
   exists**. It never runs `uv`, never creates directories. It skips
   when `VIRTUAL_ENV` is already set or `TILDE_AUTO_VENV=0`. Base
   venv provisioning is a deliberate, out-of-band action.

## Optional base venv

The base venv is a convenience — a Python environment that is active
by default in interactive shells so `pip install` (aliased to
`uv pip install`) does not touch system Python. It is opt-in:

- Provision with `uv venv "$HOME/.venv/base"` once, manually.
- Override the name with `UV_DEFAULT_VENV=<name>` in `local.fish`.
- Opt out entirely with `TILDE_AUTO_VENV=0` in `local.fish` or the
  parent environment. Use fish syntax in `local.fish`, for example:
  `set -gx TILDE_AUTO_VENV 0`.

Non-interactive fish does not auto-activate the base environment.
If the activation script is missing, startup is silent and does
nothing — the shell is still usable.

## Ownership

- **Node**: Volta owns the toolchain. `$VOLTA_HOME/bin` is prepended
  in `exports.fish`. Relative `node_modules/.bin` is *not* added to
  `PATH`; use `npx`, `pnpm exec`, or a project-local script.
- **Python**: uv owns provisioning. Only the base venv is auto-
  activated; per-project envs are the caller's responsibility.
- **Homebrew**: adds `/opt/homebrew/{bin,sbin}` on macOS. Full
  `brew shellenv` is not invoked to avoid a subprocess per shell.

## Isolated benchmarks

To compare fish startup cost across a change, use a hermetic sandbox
that does not inherit the current login environment:

```sh
# Baseline: minimal fish (no configs at all).
hyperfine --warmup 5 'fish --no-config -c "exit"'

# Current tracked config, isolated from the user's real HOME.
fish_bin=$(command -v fish)
tmp=$(mktemp -d)
mkdir -p "$tmp/fish"
cp -R fish/.config/fish/. "$tmp/fish/"
hyperfine --warmup 5 \
    --setup 'true' \
    "env -i PATH=/usr/bin:/bin HOME=$tmp XDG_CONFIG_HOME=$tmp $fish_bin -l -c 'exit'"
```

`env -i` clears the environment so results are not contaminated by
`PATH` differences between runs. Run before and after a change on
the same machine, back-to-back, and report both numbers together.

Do **not** claim a measured speedup here or in commit messages
without a hyperfine artifact attached. Numbers cited without a
reproducible harness are noise.
