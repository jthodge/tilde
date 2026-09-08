# Configuration verification

Run from the repository root:

```sh
make test-tools  # explicit installation: ShellCheck and pinned Pyright via uv
make verify      # lint -> typecheck -> tests; never installs tools or packages
make smoke       # separate offline Emacs init check, needs installed packages
make check       # validate the actual Stow deployment
make doctor      # read-only host/tool inventory
```

`make lint`, `make typecheck`, and `make test` also work separately. Each
verification command emits JSON on stdout, narration and child-tool output
on stderr, exits nonzero on failure, and stops at the first failed phase.
The Makefile records the Pyright version used locally and in CI. Pyright uses
the test interpreter's library paths while checking Python 3.9 compatibility.
CI explicitly selects Python 3.13, rather than inheriting Homebrew's newest
Python and its possibly unsupported syntax.

## Coverage and boundaries

- **Lint:** tracked fish/zsh/Bash syntax, Python/JSON/YAML parsing, Brewfile
  Ruby syntax, Lisp parentheses, Neovim Lua syntax/key declarations, whitespace.
  ShellCheck covers the maintained deployment/bootstrap/cleanup/link helpers
  and the staged-secret hook; it does not cover every legacy shell function.
- **Types:** Python doctor, seeder, verification runner, and regression tests.
  Dynamic Lisp/Lua behavior is covered by targeted tests, not a static type
  system. Existing agent TypeScript extensions are not typechecked here.
- **Preservation:** seed-only settings survive repeated runs; legacy links get
  private backups; Python link management refuses foreign files and links.
- **Failure handling:** cleanup dry runs and cancellations, invalid selections,
  nullable GitHub fields, partial failures, missing tools, and invalid configs.
  GitHub operations are mocked: tests do not delete real runs or deployments.
- **Shell:** copies only tracked fish sources, with temporary homes and command
  stubs. Local credentials and universal variables are never copied.
- **Editors:** ERT checks use fresh homes and no installed packages. The separate
  smoke test loads the full init with installed packages and temporary state,
  refusing network/package installation. Neovim checks use plugin stubs.
- **tmux:** a private socket, temporary home, stub plugin, and `sleep` process;
  repeated config loads are idempotent. No production pane is killed. This
  host-specific test is skipped when Apple Silicon fish/tmux are unavailable.
- **Git and deployment:** scanner tests inspect staged blobs, including binary
  data and type changes, without printing synthetic matches. Baseline fixtures
  use tree/index objects, not commits. Checker tests cover ownership, missing
  dependencies, malformed manifests, and Git/path inspection failures.
- **Policy:** targeted text checks prevent known signing/publication conflicts.
  They do not prove that an agent will obey prose or that heuristic permission
  guards are a sandbox. Extension boundaries and their documented limits are
  spelled out in [agent-boundaries.md](agent-boundaries.md).
- **Pi extensions:** `test_plan_mode.mjs` and `test_gates.mjs` load the real
  extension sources with `node:module.stripTypeScriptTypes` and import the
  stripped JavaScript via a data URL. Only `@earendil-works/pi-tui` is stubbed
  at runtime; every other pi import is `import type` and is erased by the
  built-in type stripper. Tests exercise the extensions through a minimal
  in-memory `ExtensionAPI`. If the stripper is unavailable the harness exits
  nonzero with a MISSING message, so the check cannot silently pass. Requires
  Node 22.13+ on the 22.x line, or Node 24. CI selects Node 22 explicitly in
  `.github/workflows/check.yml`; no Pi installation is needed.

The GitHub Actions job runs `make verify` on macOS without Stow deployment,
credentials, user data, or an editor-package bootstrap. Tool installation is
an explicit CI preparation step. The runner image/Homebrew packages are not
fully pinned, so this is reproducible test *procedure*, not a hermetic build.

## Still manual

Fresh-machine macOS bootstrap, 1Password and accessibility prompts, actual
LSP/GUI completion round trips, visual editor interactions, remote clipboard
round trips, and whether a workflow reduces daily friction. Do not infer these
from unit-test success. See [Emacs](emacs-workflow.md),
[terminal](terminal-workflow.md), and [shell](shell-startup.md) workflow notes.

Stage new sources before the final verification pass: tracked-file syntax
checks intentionally exclude ignored local configuration and application state.
Tests do not authorize a commit or a push; follow the repository's Git policy.
