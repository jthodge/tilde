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
The Makefile records the Pyright version used locally and in CI.

## Coverage and boundaries

- **Lint:** tracked fish/zsh/Bash syntax, Python/JSON/YAML parsing, Brewfile
  Ruby syntax, Lisp parentheses, Neovim Lua syntax/key declarations, whitespace.
  ShellCheck covers the maintained deployment/bootstrap/cleanup/link helpers;
  it does not claim to lint every legacy shell function or Git hook.
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
- **Policy:** targeted text checks prevent known signing/publication conflicts.
  They do not prove that an agent will obey prose or that heuristic permission
  guards are a sandbox.

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
