# tilde

Personal macOS dotfiles, deployed via [GNU Stow][stow].

## Prerequisites (manual, one-time)

Bootstrap is intentionally explicit — no target here installs
Homebrew, unlocks 1Password, or grants macOS accessibility
permissions on your behalf. Do the following by hand, once per
machine, as required for the full setup:

1. **Xcode Command Line Tools** — `xcode-select --install`.
   Supplies `git`, `/usr/bin/python3` (which `scripts/doctor` and
   the unit tests use), and the linker the Brewfile depends on.
2. **Homebrew** — install per
   [brew.sh](https://brew.sh). `make brew` then reads the
   Brewfile; it never bootstraps Homebrew itself.
3. **1Password + SSH agent** — install the desktop app, unlock the
   vault, and enable the SSH agent (Settings → Developer →
   "Use the SSH agent"). Commit signing requires it and stops on
   failure. The secret scanner runs without it. Never pass
   `--no-gpg-sign`.
4. **Private font — Berkeley Mono** — Ghostty, the sole supported
   terminal, pins Berkeley Mono at size 10 (see `AGENTS.md`). Install the
   `.ttf` payload from your licensed download; the Brewfile cannot
   distribute it.
5. **App permissions** — Rectangle, Keyboard Maestro, Alfred,
   Keycastr, and Superwhisper each need Accessibility / Input
   Monitoring granted in System Settings → Privacy & Security
   after their first launch.
6. **Volta ownership of the JS toolchain** — Volta is the sole
   owner of `node`, `pnpm`, `yarn`, and any `volta install`ed
   binaries. Do not install Node from a `.pkg`, from `nvm`, or
   from `brew install node`; the shims will fight over PATH.
   `scripts/runtime-versions.env` records the pinned defaults
   (currently Node 22.14.0, pnpm 10.15.0, yarn 4.4.0). To adopt a
   new version, edit that file **and** run
   `volta install <tool>@<version>` — `setup-tools` never rewrites
   an existing default silently.

## Bootstrap

```sh
git clone git@github.com:jthodge/tilde.git ~/tilde
cd ~/tilde
make brew         # install the Homebrew packages the Brewfile declares
scripts/setup-tools --check   # report toolchain plan, mutate nothing
make tools        # explicit: bootstrap Volta node/pnpm/yarn if absent
make              # = make dry-run: simulate the deployment, write nothing
make switch       # deploy every package; resolve conflicts before proceeding
make plugins      # explicit: init submodules + install TPM plugins
make doctor       # JSON report on stdout, human summary on stderr
make check        # verify the live $HOME against this checkout
```

A `Makefile` wraps the workflow. Run `make help` for the full target
list. `make` on its own is always safe: it simulates and writes
nothing. `make brew`, `make tools`, and `make plugins` can install
software and require an explicit invocation. No default target chains
into them. `make tools` preserves existing Volta defaults and only
creates `~/.venv/base` if absent.

Each entry in `.stow-packages` mirrors a slice of `$HOME`; the
file is the canonical list and every `make` target feeds it to
`stow` verbatim. The `scripts/` directory is intentionally not
stowed — its contents are invoked in place.

> **No clean-machine validation.** This repo is not tested against
> a fresh macOS install end-to-end. The list above is the proposed
> bootstrap procedure; expect to reconcile app-owned config
> drift (see `make check`) and permission prompts on first launch.

## Verifying a deployment

Three diagnostics answer different questions:

- `make check` (`scripts/check`) — **does the deployed `$HOME`
  match its ownership rules?** Checks repository-owned links and
  seed-only app-owned files. See [configuration ownership](docs/config-ownership.md).
  It reports:
  - `MISSING` — the package is not stowed.
  - `DRIFT` — the target exists but resolves elsewhere. An
    application replaced a repository-owned link with a real file,
    or a seed-only file has the wrong ownership.
  - `UNDECLARED` — a tracked directory that `.stow-packages`
    omits, so a fresh bootstrap would skip it.
- `make doctor` (`scripts/doctor`) — **does the surrounding
  environment satisfy the prerequisites?** Emits a JSON report on
  stdout and a short human summary on stderr. Reports required
  tools, optional editor tools separately, git submodule
  initialisation, TPM plugin installation, and every alternative
  copy of each tool on PATH (to surface shadowing). The doctor
  never executes a discovered tool, never sources shell startup,
  never reads credentials, and never mutates the tree. Requires
  `python3` from the Xcode Command Line Tools.
- `make capabilities` — **do installed language tools work on synthetic inputs?**
  Explicitly runs test runners, formatters, compilers and LSP initialization;
  installs nothing. Required failures return nonzero. See
  [development capabilities](docs/development-capabilities.md), including the
  project-interpreter override and the known missing base-environment pytest.

`make brew-diff` is the package-inventory equivalent: it lists
formulae and casks that are installed but that the `Brewfile` does
not declare.

## Configuration regression checks

```sh
make test-tools   # explicit: install ShellCheck and pinned Pyright
make verify       # lint, typecheck, tests; no provisioning
make smoke        # optional full-init Emacs check with installed packages
```

See [verification scope and limitations](docs/verification.md),
[shell startup](docs/shell-startup.md), [Emacs workflow](docs/emacs-workflow.md),
and [terminal workflow](docs/terminal-workflow.md).
[Adoption decisions and open checks](docs/adoption-decisions.md) records why
Stow and the existing personal tools remain in place. See
[reviewed upgrades and recovery](docs/upgrades-and-recovery.md) before changing
installed packages or migrating seed-only local settings.

## GitHub SSH key registration

GitHub treats SSH keys in two independent categories on your account:
**Authentication keys** (used for `git push` / `git fetch` over SSH) and
**Signing keys** (used to verify commit signatures). A key registered
under one does not count for the other.

Register `~/.ssh/id_ed25519.pub` under **both** so that:

- `git push` over SSH works (authentication).
- Commits signed locally via `op-ssh-sign` render a green **Verified**
  badge on github.com (signing).

Same public key bytes; two separate entries.

### Via the GitHub UI

At https://github.com/settings/keys, for each category:

1. Click **New SSH key**.
2. **Title:** `id_ed25519 (authentication)` or `id_ed25519 (signing)`.
3. **Key type:** select **Authentication Key** or **Signing Key** to match.
4. **Key:** paste the contents of `~/.ssh/id_ed25519.pub`.
5. Save. Repeat for the other category.

### Via the `gh` CLI

```sh
gh auth refresh -h github.com -s admin:public_key,admin:ssh_signing_key
gh ssh-key add ~/.ssh/id_ed25519.pub --type authentication --title "id_ed25519 (authentication)"
gh ssh-key add ~/.ssh/id_ed25519.pub --type signing --title "id_ed25519 (signing)"
```

### Why the separation exists

The allowlists are independent so that a read-only deploy key (auth
only, often on shared infrastructure) cannot be misused to forge
"verified" commits in your account, and a hardware-token-bound signing
key never doubles as an inbound auth vector.

## Pi

Pi is the polyglot agent harness installed alongside Claude Code. Managed
via Volta for parity with the rest of the JS toolchain:

```sh
volta install @earendil-works/pi-coding-agent
```

Check both the installed package's Node engine requirement and Volta's
package-specific runtime before upgrading. The reviewed Pi 0.85.1 package
requires Node ≥ 22.19.0; its Volta platform selects Node 24.18.0 independently
of this repo's user default, Node 22.14.0. Do not change the user default just
to make those two runtimes match. Package-manager metadata and installed
payload versions can also drift; see the [reassessment](docs/adoption-decisions.md).

Authenticate via OAuth subscriptions rather than API keys:

```sh
pi
# inside the TUI:
/login   # pick "Claude Pro/Max", complete browser OAuth
/login   # pick "ChatGPT Plus/Pro (Codex Subscription)", complete browser OAuth
```

Tokens land at `~/.pi/agent/auth.json` (runtime state, not tracked).

**Billing note:** third-party harnesses like Pi do not consume Claude
Pro/Max plan limits. Usage is billed per token as "extra usage" —
enable at https://claude.ai/settings/usage before invoking Anthropic
models from Pi.

## Hooks

The `git` package ships a staged-secret pre-commit hook at
`git/.config/git/hooks/pre-commit`. After `stow git`, `~/.gitconfig`
points `core.hooksPath` at `~/.config/git/hooks`, making the scanner
active for every commit on this host. Bypass requires the explicit
`git commit --no-verify` escape after manual review.

## References

* [Managing dotfiles with GNU stow][stow]
* [Restore, Clone or Backup your Homebrew Setup](https://tomlankhorst.nl/brew-bundle-restore-backup/)
* [`Save As -> .pdf` Keyboard Shortcut](https://www.macsparky.com/blog/2008/3/19/keyboard-shortcut-for-save-as-pdf-in-os-x.html)
* [Universal Monitor Window Switcher](https://gist.github.com/jthodge/c4ba15a78fb29671dfa072fe279355f0)

[stow]: https://alexpearce.me/2016/02/managing-dotfiles-with-stow/
