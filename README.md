# tilde

Personal macOS dotfiles, deployed via [GNU Stow][stow].

## Bootstrap

```sh
git clone git@github.com:jthodge/tilde.git ~/tilde
cd ~/tilde
make brew      # install the Homebrew packages the Brewfile declares
make           # = make dry-run: simulate the deployment, write nothing
make switch    # deploy every package
make check     # verify the live $HOME against this checkout
```

A `Makefile` wraps the workflow. Run `make help` for the full target
list. `make` on its own is always safe: it simulates and writes
nothing.

Each entry in `.stow-packages` mirrors a slice of `$HOME`; the
file is the canonical list and every `make` target feeds it to
`stow` verbatim. The `scripts/` directory is intentionally not
stowed — its contents are invoked in place.

## Verifying a deployment

`make check` (`scripts/check`) resolves every tracked file in every
declared package to its path under `$HOME` and compares the two:

- `MISSING` — the package is not stowed.
- `DRIFT` — the target exists but resolves elsewhere. An application
  replaced the link with a real file, so repo edits no longer reach
  the live config.
- `UNDECLARED` — a tracked directory that `.stow-packages` omits, so
  a fresh bootstrap would skip it.

`make brew-diff` is the package-inventory equivalent: it lists
formulae and casks that are installed but that the `Brewfile` does
not declare.

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

Volta records the pinned version in its toolchain (visible via
`volta list`). Pi 0.75+ requires Node ≥ 22.19.0; on older Node, Volta
will resolve to the latest compatible 0.74.x. Update Node via
`volta install node@22.19.0` to unlock newer Pi releases.

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
