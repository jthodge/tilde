# macOS Nix migration

## Current checkpoint: Ghostty bridge

This is an incremental, behavior-preserving migration, not an application or
runtime redesign. The current flake targets Apple Silicon macOS
(`aarch64-darwin`), user `jth`, home `/Users/jth`, and checkout `~/tilde`.
Fresh-machine bootstrap has not been validated end-to-end.

| Resource | Current deployment or installation owner |
| --- | --- |
| Packages in `.stow-packages` | Stow, through `make switch` |
| Ghostty configuration | Standalone Home Manager, through `home.nix` |
| Ghostty and Fish executables | Homebrew |
| Node, pnpm, Yarn | Volta |
| Python installations and environments | uv / project |
| Nix installation and daemon | Determinate Nix |

`.home-manager-packages` records checkout-layout packages transferred to the
out-of-store bridge. It does not generate Home Manager declarations; `home.nix`
does that. Each package belongs to exactly one deployment manifest. These
manifests describe the intended owner, not evidence that activation succeeded.
Home Manager also manages the demonstration file and its default housekeeping
links, profiles, and dedicated application/font directories.

The Ghostty link chain is now:

```text
~/.config/ghostty/             real directory
  config -> Home Manager store links -> ~/tilde/ghostty/.config/ghostty/config
```

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
Ghostty is expected to be missing from `make check` until Home Manager activation.

## One-time handoff from the old Stow layout

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

## Recovery boundaries

**Before any live Home Manager activation:** return `ghostty` to `.stow-packages`
and remove it from `.home-manager-packages`. Preview restoration:

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
`make verify` does not build Nix or exercise live Home Manager activation.
Checker tests cover bridge source resolution and duplicate ownership, not the
Home Manager activation engine or a fresh macOS bootstrap.

Next, audit Fish before changing it: its directory-level Stow link also exposes
ignored universal-variable state. Preserve that state and local overrides;
keep Homebrew Fish, Volta, uv, and the existing startup phase order unchanged.
Do not combine this handoff with `programs.fish` conversion or a shell redesign.
