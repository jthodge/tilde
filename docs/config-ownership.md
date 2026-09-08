# Configuration ownership

Each target has one owner. A Git checkout restores tracked configuration,
not application data, credentials, package installations, or local preferences.

| Target | Owner | Deployment | Runtime writes and recovery |
| --- | --- | --- | --- |
| Shell, Git, terminal and tmux config | Repository | Stow links | Edit the repository; use Git to restore intended content |
| Shared agent instructions and skills | Repository | Stow links | Edit the canonical source; restart sessions to load it |
| `~/.claude/settings.json` | Claude | Seed once, regular local file | Preserve local model, plugin and permission choices; template changes do not update existing files |
| `~/.claude/settings.local.json` | Local user / Claude | Unmanaged | Never publish local permissions or credentials |
| `~/.codex/config.toml` | Codex | Unmanaged | Keep local choices; no gateway or mentor-specific defaults are imposed |
| `~/.pi/agent/settings.json` | Repository and Pi | Intentional writable link | Review saved preferences/package changes in Git; keep credentials and sessions outside Git |
| Pi extension and theme sources | Repository | Stow links | Treat as maintained code; test changes before a new session |
| `~/.config/nvim/lazy-lock.json` | Repository and lazy.nvim | Intentional writable link | Review plugin updates and commit the tested lockfile |
| Neovim plugin installations | lazy.nvim | Unmanaged cache | Restore from the tracked lockfile; not from a full config checkout |
| `~/.emacs.d/` source | Repository | Stow links | Edit modules in the repository |
| Emacs Custom, packages, native cache, backups | Emacs | Ignored local state | Keep durable settings in modules; do not commit generated files |
| fish prompt sources | Repository | Stow links | Vendored Tide remains tracked; update it as a separate reviewed change |
| fish universal variables and `local.fish` | fish / local user | Ignored local state | Do not make bootstrap depend on undeclared universal variables |
| TPM | Git submodule | Explicit `make plugins` | Restore the submodule revision, then install declared plugins |
| TPM plugin checkouts | TPM | Ignored runtime installs | `make plugins` installs them; updates are separate from Stow |
| Node, pnpm and yarn | Volta | Explicit `make tools` | Manifest supplies defaults only when absent; existing versions are preserved |
| Python installations and environments | uv / project | Explicit setup | Project files own project dependencies; base environment is optional convenience |
| API credentials and signing keys | 1Password | Inject at point of use | Never seed or copy secret values from this repository |
| `.context/`, agent sessions and logs | Running tools | Ignored local state | Disposable unless a separate retention policy says otherwise |

## Claude settings migration

`claude/.stow-local-ignore` excludes the tracked settings template. After
link deployment, `make switch` runs `scripts/seed-configs --apply`:

- Missing file: validate the template JSON and create a private regular file.
- Existing regular file: do not read, rewrite, merge, or chmod it.
- Legacy symlink: validate its JSON, back up its contents under
  `~/.local/state/tilde/backups/`, then replace only the link with a private file.
- Broken link or non-file target: stop. Do not silently reset preferences.

`make dry-run` reports the proposed seed action without applying it.
`make check` checks the target's ownership, not equality to the template.
`make unstow` deliberately leaves app-owned settings in place.

To change an existing machine's defaults, review the local/template difference
and apply a specific migration. Never copy the whole template over local state.
Older revisions may expect a settings symlink. Preserve local settings before
rolling back across this ownership change.
