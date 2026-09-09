# Reviewed upgrades and recovery

Git restores source configuration, not installed packages or app-owned state.
Use the matching owner below; never back up a whole agent directory containing
credentials and sessions. No live upgrade or settings migration was performed
while adding these procedures.

## Claude settings

`make switch` still preserves existing regular settings byte-for-byte.
`scripts/migrate-claude-settings` is a separate, explicit migration of
`~/.claude/settings.json`:

- `sonnet-defaults-v1` changes a missing model or exactly `opus[1m]` to `sonnet`.
- It adds missing `modelSettings.claude-sonnet-5.effortLevel: high`.
- Custom models, existing effort choices and unrelated fields remain intact.
- Ambiguous null/non-object settings, duplicate keys, non-finite numbers,
  oversized files and symlinks are refused.
- A no-op preserves exact bytes and creates no backup. A real change preserves
  other values semantically but reserializes JSON whitespace/escaping.

**Quit all Claude instances normally before applying or restoring.** Confirm
that no daemon or other writer remains. The identity/content recheck is not an
atomic compare-and-swap against hostile writers; parent directories are trusted.

```sh
make migrate-claude
python3 scripts/migrate-claude-settings --apply \
  --expected-sha256 "EXPECTED_SHA256_FROM_PREVIEW"
```

Replace the quoted placeholder with the preview's `expected_sha256`. Reports
contain fixed action/field labels and computed hashes, not settings values or
unknown keys. Inspect sensitive details privately, not in shared diffs/logs.
Changed state after preview causes refusal rather than an implicit new plan.

Before publication, original bytes and integrity metadata are saved under
`~/.local/state/tilde/claude-migrations/`: directories `0700`, files `0600`.
Keep the printed backup ID. Preview and apply a restore separately:

```sh
python3 scripts/migrate-claude-settings --restore "BACKUP_ID"
python3 scripts/migrate-claude-settings --restore "BACKUP_ID" --apply \
  --expected-sha256 "EXPECTED_SHA256_FROM_RESTORE_PREVIEW"
```

Restore refuses later app edits. For a missing target, the preview still supplies
`expected_sha256`; exclusive publication preserves a concurrently created file.
There is no force flag. Restore verifies stored bytes and metadata before use.
Hashes detect corruption, not an attacker who rewrites both backup and metadata.
These are local backups, not a crash journal or an off-machine backup service.
Future migrations must retain the v1 reader and exact v1 serialization, rather
than changing the existing migration identifier's meaning.

## Emacs packages

`my/install-packages` installs missing packages; it does not upgrade them.
There is no ELPA version lock here. An artifact snapshot can recover this host's
package tree, but does not provide a pinned fresh-machine reconstruction.

Save buffers and quit all Emacs instances/daemons normally before snapshot or
restore. Confirm they and their compiler jobs have stopped; process names vary
between GUI, CLI and daemon installations. Do not kill processes blindly.

### Snapshot

This copies only the regular `elpa` directory into persistent private state.
Nested symlinks and executable bits are retained. External symlink targets are
not copied. Keep the printed path until the upgrade is verified.

<!-- snapshot-elpa:begin -->
```sh
set -eu
umask 077
elpa="$HOME/.emacs.d/elpa"
if [ -L "$elpa" ]; then
  echo "refuse: elpa is a symlink" >&2
  exit 1
fi
if [ ! -e "$elpa" ]; then
  echo "no elpa directory" >&2
  exit 1
fi
if [ ! -d "$elpa" ]; then
  echo "refuse: elpa is not a directory" >&2
  exit 1
fi
snap_parent="$HOME/.local/state/tilde/upgrades"
if [ -L "$snap_parent" ]; then
  echo "refuse: backup parent is a symlink" >&2
  exit 1
fi
mkdir -p "$snap_parent"
chmod 0700 "$snap_parent"
snap_root="$(mktemp -d "$snap_parent/elpa-XXXXXX")"
chmod 0700 "$snap_root"
snap="$snap_root/elpa"
cp -Rp "$elpa" "$snap"
printf 'snapshot=%s\n' "$snap"
printf 'snap_root=%s\n' "$snap_root"
```
<!-- snapshot-elpa:end -->

### Review and upgrade

Open a fresh Emacs, then `M-x list-packages` to refresh/list candidates.
`U` marks upgrades; `u` unmarks unwanted rows. Review the marked set before `x`.
Afterward restart Emacs, run `make smoke`, and exercise your real projects.
Do not confuse installation of missing packages with an upgrade.

### Restore

Quit Emacs again. Set `snap` to the printed snapshot path. This moves the failed
installation into the snapshot directory before restoring. An existing failed
slot is never overwritten. Stop on any copy error: the snapshot and failed tree
remain available, but a partial restored tree must be inspected before restart.

<!-- restore-elpa:begin -->
```sh
set -eu
elpa="$HOME/.emacs.d/elpa"
if [ ! -d "$snap" ] || [ -L "$snap" ]; then
  echo "snapshot missing or symlinked" >&2
  exit 1
fi
if [ -L "$elpa" ]; then
  echo "refuse: elpa is a symlink" >&2
  exit 1
fi
if [ -e "$elpa" ] && [ ! -d "$elpa" ]; then
  echo "refuse: elpa is not a directory" >&2
  exit 1
fi
snap_root="$(dirname "$snap")"
failed="$snap_root/elpa.failed"
if [ -e "$failed" ] || [ -L "$failed" ]; then
  echo "refuse: failed snapshot already exists" >&2
  exit 1
fi
if [ -e "$elpa" ]; then
  mv "$elpa" "$failed"
  printf 'failed_elpa=%s\n' "$failed"
fi
cp -Rp "$snap" "$elpa"
```
<!-- restore-elpa:end -->

The blocks are rehearsed verbatim against synthetic package trees, including
copy failures. They are not a concurrent-writer transaction. Emacs itself,
native caches and system libraries are outside this rollback. Diagnose and
quarantine suspect caches separately; never delete Custom, backups or recovery
files as a blanket reset. Restore does not undo already-loaded code: restart.

## Neovim

Review plugin updates in `:Lazy`, then inspect/test the changed
`nvim/.config/nvim/lazy-lock.json` before committing it. Restore a previously
reviewed lockfile from Git, then run `:Lazy restore`. This may fetch revisions;
the lockfile is not an installed-artifact backup. Preserve the plugin cache
separately if offline recovery is required. No real update/restore was run here.

## Other owners and limits

| Owner | Recovery boundary |
| --- | --- |
| Pi | Git restores tracked settings/extensions, not installed packages, credentials or sessions. |
| TPM | Record plugin commit IDs before updates. `make plugins` installs missing checkouts; it does not roll back existing ones. The TPM submodule pins TPM, not every plugin. |
| Volta | Record prior defaults and explicitly select reviewed versions to return to them. `make tools` preserves existing defaults; it is not rollback. |
| Homebrew | `Brewfile` declares a package set, not historical binaries. Reinstalling/bundling is not version rollback. |
| Python projects | Project manifests/locks own dependencies. The optional base environment is not a project recovery artifact. |
| Local Custom/history/recovery files | Preserve privately; a Git source rollback does not restore them. |
| Credentials | 1Password; never copied by these procedures. |

See [config-ownership.md](config-ownership.md) and
[verification.md](verification.md). A successful fixture rehearsal does not
prove an actual package upgrade, cross-version recovery or fresh-host rebuild.
