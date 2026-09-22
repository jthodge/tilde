# macOS Nix migration

## Current checkpoint

`.home-manager-packages` and `.stow-packages` are the current ownership inventory.
Transfers are committed package by package. Runtime installation ownership and
application state locations remain unchanged.

This is an incremental, behavior-preserving migration, not an application or
runtime redesign. The current flake targets Apple Silicon macOS
(`aarch64-darwin`), user `jth`, home `/Users/jth`, and checkout `~/tilde`.
Fresh-machine bootstrap has not been validated end-to-end.

| Resource | Current deployment or installation owner |
| --- | --- |
| Packages in `.stow-packages` | Stow, through `make switch` |
| Ghostty configuration | Standalone Home Manager, through `home.nix` |
| Bash and Zsh configuration | Standalone Home Manager, through `home-shells.nix` |
| Bin helper and SSH config/public signers | Standalone Home Manager, through `home-files.nix` |
| Preserved file/directory boundaries | Standalone Home Manager, through `home-bridges.nix` |
| Ghostty and Fish executables | Homebrew |
| Node, pnpm, Yarn | Volta |
| Python installations and environments | uv / project |
| Nix installation and daemon | Determinate Nix |

`.home-manager-packages` records checkout-layout packages transferred to the
out-of-store bridge. It does not generate Home Manager declarations; `home.nix`
and its imported modules do that. Each package belongs to exactly one deployment
manifest. These manifests describe the intended owner, not evidence that
activation succeeded.
Home Manager also manages the demonstration file and its default housekeeping
links, profiles, and dedicated application/font directories.

The Ghostty link chain is now:

```text
~/.config/ghostty/             real directory
  config -> Home Manager store links -> ~/tilde/ghostty/.config/ghostty/config
```

Bash and Zsh use individual links for `.bash_profile`, `.spaceshiprc.zsh`,
`.zprofile`, `.zshenv`, and `.zshrc`, pointing back to their original checkout
sources. No generated shell initialization or Nix-managed shell packages are
introduced.

Edit the checkout source directly, then reload Ghostty. The bridge preserves
live edits without rebuilding Nix. Do not edit store outputs. Do not put secrets
in Nix expressions or store-backed sources. Local mutable state stays outside
Home Manager declarations unless a separate ownership decision says otherwise.

`flake.lock` pins dependencies; `home.stateVersion = "26.05"` is a compatibility
baseline, not a dependency pin. No nix-darwin configuration is active. If it is
introduced while Determinate owns Nix, use `nix.enable = false` and avoid two
activation entry points competing for the same Home Manager configuration.

## Build, inspect, preview, activate

Install Determinate Nix explicitly using its official installer before this
workflow; `make` does not install it. Use a fresh terminal where `nix` is on PATH.
These commands use **Fish**, from `~/tilde`:

```fish
set hm_build (nix build --no-link --print-out-paths \
  '.#homeConfigurations.jth.activationPackage')
```

Stop if the build fails or produces no path. Building writes/downloads store
artifacts but does not activate the home configuration. Inspect the candidate:

```fish
realpath "$hm_build/home-files/.config/ghostty/config"
shasum -a 256 "$hm_build/home-files/.config/ghostty/config"
```

The resolved path must be the checkout source. Compare the hash with the
reviewed source; a changed hash is not automatically wrong after intentional
edits. If `~/.config/ghostty` is still a Stow directory symlink, perform the
handoff below before activation. Never force through an existing target.

Preview, inspect its output, and only then activate the same artifact:

```fish
env DRY_RUN=1 VERBOSE=1 "$hm_build/activate"
```

```fish
env VERBOSE=1 "$hm_build/activate"
echo $status
```

Use neither `sudo` nor blanket `force = true`. Home Manager's dry-run can perform
initialization bookkeeping; it is not a filesystem sandbox. Live activation is
not transactional: stop and inspect any failure rather than blindly retrying or
restoring over partially deployed files.

After exit status `0`, verify both the active generation and link topology:

```fish
realpath "$HOME/.local/state/nix/profiles/home-manager"
command ls -ld "$HOME/.config/ghostty"
readlink "$HOME/.config/ghostty/config"
realpath "$HOME/.config/ghostty/config"
shasum -a 256 "$HOME/.config/ghostty/config"
make check
make doctor
```

The profile must resolve to the reviewed artifact, the parent must be a real
directory, and the file must link through its Home Manager `home-files` tree
back to the intended checkout source. Reload Ghostty and check a new tab for
Fish startup, appearance, and Option/Alt behavior. Repeat activation to check
that an unchanged artifact reuses the generation. It can still relink files;
idempotence does not mean no commands execute.

`make switch` only restows the remaining Stow packages and applies seed-only
configuration. It does **not** build or activate Home Manager. On a fresh home,
targets from `.home-manager-packages` will be missing from `make check` until
Home Manager activation.

## Preserving existing directory boundaries

A directory bridge changes its deployment owner without relocating its contents.
`home-bridges.nix` uses absolute out-of-store source strings and `recursive = false`.
Ignored mutable files remain outside the store, in their original locations.
Never substitute a directory copy, a `path:` flake snapshot of the dirty checkout,
or file-by-file state relocation for this bridge.

For a reviewed candidate, first pin both it and the previous generation with
`nix-store --realise GENERATION --add-root RECORD/candidate --indirect` (and a
separate `RECORD/previous` root). Keep the record outside Git. The roots protect
link destinations and recovery artifacts from garbage collection.

Preview `python3 scripts/adopt-home-links --generation GENERATION --target TARGET`
(repeat `--target` for each boundary). Save its JSON metadata. Only when every
current and candidate link resolves to the same path and inode, apply with
`--apply`. The helper refuses regular files, escaping or symlinked parents,
overlapping targets, and different sources. It stages a symlink beside the target
and uses `os.replace`, so the link is not deliberately absent between managers.
It never reads or copies application state. Multiple link replacements and Home
Manager activation are not one transaction; stop and inspect any failure.

Then preview and activate the pinned generation. Check immediate Home Manager
link targets, resolved source identity, source hashes, and `make check`. Do not
restart applications just to change link ownership. For a failed pre-activation
handoff, the saved original link strings can be restored with the same guarded,
atomic same-source replacement; keep both GC roots until recovery is verified.
After activation, recovery must also reconcile the active generation and package
manifests. Never blindly restore older source contents or state snapshots.

This does not move Fish universal variables, Emacs packages/caches, Neovim state,
or tmux plugins. Separating that mutable state is a later, explicit migration
that may still need a quiet window. Shared state directories must never be
replaced by per-file declarations while writers are active.

### Package checkpoints

| Package | Preserved boundaries | Verification |
| --- | --- | --- |
| agents | `.agents` | Same directory inode and source paths; regression suite |
| claude | `CLAUDE.md`, commands directory | Local settings remain regular and untouched; seeding still runs through `make switch` |
| codex | `.codex/AGENTS.md` | Same canonical preferences; config, authentication, and sessions unmanaged |
| emacs | `.emacs.d` | Same directory inode and tracked sources; 58 ERT tests and fresh-home smoke; no editor restart or state relocation |
| git | `.gitconfig`, ignore file, hooks directory | Same source hashes and signing trust; unrelated hook edits excluded from the migration commit |

## Bin and SSH handoff

`home-files.nix` declares only `.local/bin/uv-python-simlink`, `.ssh/config`,
and `.ssh/allowed_signers`. All three were direct file symlinks, so the same
reviewed replacement procedure as Bash/Zsh applies: transfer `bin` and `ssh`
between manifests, preview activation, then activate and verify each link.
Do not unlink or replace `.local/bin` or `.ssh` themselves.

The helper remains executable through its source link. Its behavior is tested
with fake uv installations; do not run it against live Python installations
merely to check deployment. The SSH change does not generate configuration or
keys. Preserve `.ssh` mode `0700`; private keys, known hosts, and Conductor's
included config stay local and outside the managed-file declarations.
`git verify-commit HEAD` checks that the public signing trust file still works
without accessing private signing keys or opening a network connection.

The pre-handoff checkpoint is commit `eaf494b`, with generation
`/nix/store/4vmy3wpbiqaw3v8hr76ncrd0q93i2b8n-home-manager-generation`.
The reviewed bin/SSH generation is
`/nix/store/x0z5pgn9y5b2gw8azkk0wqj9x2jl16ix-home-manager-generation`.

For recovery across this checkpoint, preview and activate the reviewed prior
configuration, checking that it releases only these three new links plus
expected housekeeping updates. Return `bin` and `ssh` to Stow's manifest and
remove them from Home Manager's manifest. Preview
`stow --simulate --verbose --dir . --target "$HOME" bin ssh`, and only apply
without `--simulate` when its scope is correct and there are no conflicts.
Recheck source hashes, executable access, SSH directory permissions, commit
signature verification, and `make check`. Preserve declarations from later
migrations when rebuilding a package-specific rollback configuration.

## Bash and Zsh handoff

These five targets were already direct file symlinks under a real home directory,
with no mutable state being relocated. After inspecting the candidate and
verifying source identity, move `bash` and `zsh` from `.stow-packages` to
`.home-manager-packages` and confirm `make -n switch` excludes them.

For this reviewed layout, Home Manager's collision check accepts identical
contents and its link step replaces existing symlinks. A separate Stow unlink
is unnecessary; avoid leaving startup files absent while activation runs.
Preview and activate the reviewed generation, then inspect each immediate link
and resolved source. An "identical" diagnostic alone is not proof of ownership.
Do not generalize this procedure to directory symlinks, regular files, or
conflicting contents, and do not enable blanket force.

The pre-handoff checkpoint is commit `8b07010`, with Ghostty-only generation
`/nix/store/9ksmkamd2h3mn613qpf0jirhpi6qvpnb-home-manager-generation`.
The reviewed Bash/Zsh generation is
`/nix/store/4vmy3wpbiqaw3v8hr76ncrd0q93i2b8n-home-manager-generation`.

To recover across this checkpoint, first preview and activate the reviewed
pre-handoff configuration, verifying it releases only the five Bash/Zsh links
in addition to expected housekeeping updates. Return `bash` and `zsh` to Stow's
manifest and remove them from Home Manager's manifest. Preview restoration with
`stow --simulate --verbose --dir . --target "$HOME" bash zsh`; only apply without
`--simulate` if its scope is correct and there are no conflicts. Verify source
hashes and `make check`. Later migrations may require rebuilding a rollback
configuration that retains their declarations rather than using this old
artifact. No source-content rollback is implied.

## One-time Ghostty handoff from the old Stow layout

Do not repeat this on an already migrated home. Keep an existing terminal open
and avoid reloading Ghostty while its configuration is temporarily absent.

1. Record the existing directory link, source hash, manifest placement, and
   current Home Manager generation before changing anything. On an existing
   Home Manager installation, capture the generation with:

   ```fish
   set hm_before (realpath "$HOME/.local/state/nix/profiles/home-manager")
   printf '%s\n' "$hm_before"
   ```

   Keep the path in the migration record, not just a transient shell variable.
   The original demonstration-only configuration is commit `a536a8f`; its
   reviewed generation was
   `/nix/store/s0ik99mc5v2aw51v8k1c5m4d860s003z-home-manager-generation`.
   Store paths may later be garbage-collected; verify availability before use.
2. Build and inspect the candidate. Move `ghostty` from `.stow-packages` to
   `.home-manager-packages`. Confirm `make -n switch` excludes it. At this point,
   `make check` can still pass against the old link: it checks source identity.
3. Preview removal of only the Ghostty link:

   ```fish
   stow --simulate --verbose --delete --dir . --target "$HOME" ghostty
   ```

   For the reviewed old layout, the only operation is `UNLINK: .config/ghostty`.
   Stop if the scope differs. Apply by removing `--simulate`.
4. Confirm the destination is absent, including no dangling symlink, and that
   the checkout source hash is unchanged. Preview and activate Home Manager as
   above. A missing-Ghostty report during the gap is expected.

## Ghostty recovery boundaries

The following describes the original Ghostty checkpoint. After subsequent
migrations, an older generation can also remove their managed links. For a
Ghostty-only rollback, rebuild a reviewed configuration that retains unrelated
current declarations rather than blindly activating the original artifact.

**Before any live Home Manager activation of the Ghostty candidate:** return
`ghostty` to `.stow-packages` and remove it from `.home-manager-packages`.
Preview restoration:

```fish
stow --simulate --verbose --dir . --target "$HOME" ghostty
```

Only if it proposes Ghostty links without conflicts, apply without `--simulate`
and run `make check`. Do not force through unexpected files.

**After live activation:** first select the recorded, reviewed pre-handoff Home
Manager generation that does not declare Ghostty. Do not capture the current
profile and mistake it for that older generation. If the artifact is unavailable,
stop and rebuild the reviewed pre-handoff configuration before continuing.
Preview its activation with `DRY_RUN=1`, inspect the removal scope, then activate
it normally. Merely switching the profile pointer does not update home links.

Confirm Home Manager released `~/.config/ghostty/config`. Remove the parent with
`rmdir` only if it is a real, empty directory; preserve any unexpected contents
and stop. Then restore the manifests and Stow links using the procedure above.
Check the source hash, deployment, and application behavior again. A failed or
partial activation requires inspecting actual links before choosing recovery.

Generation rollback restores declarations and store artifacts, **not mutable
checkout contents**. Source recovery is a separate, reviewed Git operation.
Application settings, installations, and credentials have their own owners.

## Verification and next boundary

The Ghostty checkpoint passed `make verify`, all 148 live deployment entries,
`make doctor`, repeated activation, and manual Ghostty reload/new-tab checks.
The Bash/Zsh handoff also verified all five Home Manager link targets, unchanged
source hashes, repeated activation, and the existing isolated shell regression
suite. It did not require restarting Fish or its running jobs.
The bin/SSH handoff verified the three link targets, unchanged source hashes,
helper executable access, preserved parent directories and permissions, commit
signature verification, and repeated activation. No live helper execution or
SSH connection was used as a deployment test.
`make verify` does not build Nix or exercise live Home Manager activation.
Checker tests cover bridge source resolution and duplicate ownership, not the
Home Manager activation engine or a fresh macOS bootstrap.

Fish state relocation remains deferred while mission-critical jobs run. Its
existing directory boundary can instead be preserved with a same-target bridge;
that changes ownership without moving universal variables or local overrides.
Keep Homebrew Fish, Volta, uv, and startup order unchanged. Do not combine the
bridge with `programs.fish` conversion or a shell redesign. Retain Stow until
every remaining package has a verified replacement owner.
