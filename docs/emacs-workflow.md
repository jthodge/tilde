# Emacs workflow

## Explicit modules

`emacs/.emacs.d/init.el` loads modules in a fixed order. It does not scan a
directory, so adding a file does not silently change startup behavior.

- `bootstrap`: process paths and helper functions.
- `environment`: GUI-only PATH import (see below); loads before any
  module that consults `exec-path` or `process-environment`.
- `interface`: display, editing defaults, initial minibuffer setup.
- `packages`: package declarations. **No refresh, no install at startup.**
- `lsp`: performance settings, shared language-server configuration,
  and the single completion hook installed on `lsp-mode-hook`.
- `treesitter`: grammar sources and mode mapping. **No downloads at startup.**
- `development`: completion, formatters, snippets and debugger defaults.
- `bindings`: global command bindings.
- `environments`: Python venv helpers (used by `python.el` on entry).
- `typescript`, `python`, `go`, `elisp`: language-specific setup.
- `custom-settings`: durable Customize declarations (tracked).

`init.el` sets `custom-file` to `~/.emacs.d/custom.el` early, then
loads that file **last**, after every module. `custom.el` is
gitignored; durable Customize declarations live in
`modules/custom-settings.el` and are tracked.

## Startup does no network

Two things that used to happen on every startup have been removed:

1. **Package refresh and install.** `packages.el` declares the required
   set but neither refreshes archives nor installs anything on load. A
   plain message lists missing packages if any are declared but not
   installed.
2. **Tree-sitter grammar download and build.** `treesitter.el` registers
   grammar sources into `treesit-language-source-alist` but never calls
   the installer at load. Mode remaps and file associations are added
   **only** for grammars that are already available in the current
   Emacs; other file extensions fall back to their classic mode.

The tracked startup no longer downloads packages or grammars. This does
not restrict network use by an explicitly invoked tool, language server,
or machine-local customization. The smoke test blocks URL retrieval.

### Installing / updating

Interactive commands (run inside a real Emacs, once):

- `M-x my/install-packages` - install every declared package that is
  missing. With `C-u` prefix, refresh archive contents first.
- `M-x my/install-treesitter-grammars` - build every declared tree-sitter
  grammar that is missing. Requires network and a C toolchain.
- `M-x my/import-shell-env` - re-run the GUI PATH import on demand.

## GUI PATH import

`modules/environment.el` calls `exec-path-from-shell` only when:

- `noninteractive` is nil (never in batch), **and**
- `(display-graphic-p)` is non-nil (GUI Emacs, not TTY Emacs), **and**
- `exec-path-from-shell` is installed.

Only three variables are imported: `PATH`, `CPATH`, `LIBRARY_PATH`.
No shell secrets are imported, ever.

## Completion setup

There is exactly one `completion-at-point-functions` provider chain
per LSP buffer:

- `lsp-completion-at-point` as the primary CAPF, with Cape helpers
  (`cape-yasnippet`, `cape-dabbrev`, `cape-file`) appended when
  installed.

The chain is installed by `my/lsp-completion-setup` on `lsp-mode-hook`
in `modules/lsp.el`, **before** any language module has a chance to
call `lsp-deferred`. Language modules (`typescript.el`, `python.el`,
`go.el`) contain no completion callback of their own. This is what
we test.

`lsp-completion-provider` is set to `:capf` once, in the shared
`setopt` block; there is no per-language override to `:none`.
lsp-mode is preserved as-is; Eglot is not used.

## TypeScript language server discovery

`my/typescript-configure-server` in `modules/typescript.el` picks a
`typescript-language-server` executable in this order:

1. Project-local: `node_modules/.bin/typescript-language-server` at
   any ancestor of the current file.
2. Fallback: `executable-find "typescript-language-server"` on
   `exec-path`.

Both client settings are buffer-local, including on the first call before
the client library loads. One project's server does not become the global
default for another project.

If neither is found, a plain message is emitted; lsp-mode's own
resolution runs, but the user sees an unambiguous startup message
rather than a silent failure. `lsp-clients-typescript-tls-path` and
`lsp-clients-typescript-prefer-use-project-ts-server` are the exact
variable names used by the currently installed `lsp-mode`; they were
picked by inspecting `lsp-javascript.el` in
`emacs/.emacs.d/elpa/lsp-mode-<version>/`.

## Python venv helpers

`modules/environments.el` provides:

- `uv-activate-project-buffer` - hook-safe, silent. Called by
  `python-mode-hook` **before** `lsp-deferred`, so pyright sees the
  right interpreter on its first connection.
- `uv-activate` - interactive. Preference: project `.venv`, then
  `$HOME/.venv` as an optional manual fallback. Never prompts for a
  project.
- `uv-deactivate` - interactive. Restores the pre-activation snapshot
  exactly.

The helpers make `process-environment`, `exec-path`, and
`python-shell-interpreter` buffer-local before mutating them.
Switching A -> B in a buffer first rolls back to the pre-A snapshot,
so `exec-path` never accumulates entries across successive
activations. Deactivation restores the original snapshot verbatim.

## Tests and limits

Static Python tests (already existed):

```sh
python3 -m unittest discover -s scripts/tests -p 'test_*.py'
```

New ERT tests (batch, hermetic):

```sh
emacs -Q --batch -l scripts/tests/emacs-config.el \
      -f ert-run-tests-batch-and-exit
```

The ERT tests **never** load the user's full `init.el`. They load
individual modules under stubs for `lsp-mode` and friends and never
launch a real language server. Test stubs are grouped in one section
of `scripts/tests/emacs-config.el` so it is easy to audit what is
faked and what is real.

They do **not** validate real-server behaviour: they exercise the
correctness of the Emacs config's own logic (single CAPF hook, no
duplicated language callbacks, project TS discovery, venv A -> B ->
deactivate snapshot semantics, no-project fallback, grammar
registration without an installer call, no startup package refresh).
Full real-server validation still needs a live Emacs and a real
project. This is disclosed here on purpose.

A separate full-init smoke check uses the installed packages with a temporary
HOME, Custom file, and cache. It rejects package installs and URL retrieval:

```sh
emacs -Q --batch -l scripts/tests/emacs-smoke.el
```

This check loads the real configuration but does not open projects or launch
servers. Customize writes go to local `custom.el`; backup and autosave files
go to ignored state directories. Autosave and backups remain enabled.
After installing packages or grammars explicitly, restart Emacs to apply all
conditional hooks and mode mappings.

## Mechanical extraction record

The initial split added a lexical-binding header to each module and changed
only the loader structure and separator whitespace. Removing the added
headers and restoring one separator newline after each module reproduced
the original 832-line `init.el` byte-for-byte:

```text
SHA256 9a497547c0068f6b3a9c67dc0bea8b55377c33eb284b5d2d1fc98e85de87b624
```

This verifies the initial extraction, not every possible effect of file
boundaries or subsequent behaviour changes. The step-6 correctness work
above is intentional divergence from that hash. Do not treat the
extraction hash as an expected state for the current tree.
