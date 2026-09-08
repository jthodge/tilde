# Emacs workflow

## Explicit modules

`emacs/.emacs.d/init.el` loads modules in a fixed order. It does not scan a
directory, so adding a file does not silently change startup behavior.

- `bootstrap`: process paths and helper functions.
- `environment`: GUI-only PATH import (see below); loads before any
  module that consults `exec-path` or `process-environment`.
- `proj-context`: shared per-file project resolver used by
  `workflow` and `development` (see "Shared project context" below).
  Pure helper; no side effects on load.
- `interface`: display, editing defaults, initial minibuffer setup.
- `packages`: package declarations. **No refresh, no install at startup.**
- `lsp`: performance settings, shared language-server configuration,
  and the single completion hook installed on `lsp-mode-hook`.
- `treesitter`: grammar sources and mode mapping. **No downloads at startup.**
- `development`: completion, formatters, snippets and debugger defaults.
- `bindings`: global command bindings.
- `environments`: Python venv helpers (used by `python.el` on entry).
- `typescript`, `python`, `go`, `elisp`: language-specific setup.
- `workflow`: focused pilot for project, git, search, and test bindings
  (see "Workflow pilot" below).
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

## Workflow pilot

`modules/workflow.el` provides an **initial workflow** for day-to-day project
navigation, git, search, and test running. It follows conventional key
discipline (no Evil, prefixes stay under `C-c`) and does nothing on
file open. Everything runs from explicit commands.

### Keys

| Prefix | Purpose | Notes |
| --- | --- | --- |
| `C-c p` | Built-in `project-prefix-map` (project.el) | Nothing added on top |
| `C-c g` | Sparse Magit map: `s` status, `l` log, `b` blame | Autoload targets only |
| `C-c s` | Consult search: `l/L` line(s), `r` ripgrep, `g` grep, `f` find, `i` imenu, `o` outline | |
| `C-c t` | Project tests: `p` project, `f` file, `n` nearest, `r` rerun | See below |
| `C-.` | `embark-act` | Bound only if the global slot is unbound |
| `C-;` | `embark-dwim` | Bound only if the global slot is unbound |

`which-key` turns on when installed so the prefix maps are
discoverable.

### Magit scope

The `C-c g` map exposes three autoloaded entry points. It does **not**
disable or weaken any Magit safety:

- Commit signing (`magit-commit-arguments`, GPG/SSH agent) is left as
  Magit configures it.
- Push confirmation prompts and "dangerous action" warnings are not
  suppressed.
- Git continues to require SSH signing through the 1Password agent.

Treat the map as a shortcut, not a policy override.

### Project tests

`C-c t` runs test commands via `compile` (compilation-mode) with
`default-directory` pinned to the **same resolved scope root** for
all three scopes and for rerun (see "Shared project context" below).
The scope root is the language-specific `:root` from
`my/proj-context` when a buffer file resolves one, else the outermost
VC root. Using one root per (buffer-file, project) means the
per-project last-command hash is consistent across scopes: running
file tests then rerun no longer looks up an inner root while the
entry was stored under an outer root.

Language detection follows the **current buffer's file extension**
(then `major-mode` when the buffer has no file). It is **not**
inferred from marker files: a `.ts` file in a mixed repo that also
carries `pyproject.toml`, `package.json`, `pnpm-lock.yaml`, and
`go.mod` runs `pnpm test`, never `pytest`. Marker-based detection is
reserved for the case where the buffer has no file at all; an
ambiguous multi-language root in that case signals `user-error` and
tells you to open a source file or set the override. A buffer with
an unknown extension (`.txt`, `.rst`, ...) also fails closed: `C-c
t` refuses rather than sliding into marker inference.

| Kind | Extension | Project | File | Nearest |
| --- | --- | --- | --- | --- |
| Python | `.py`, `.pyi` | `uv run pytest` | `uv run pytest <file>` | `uv run pytest <file>::<class>::<test>` |
| Go | `.go` | `go test ./...` | `go test ./<pkgdir>` | `go test -run '^TestX$' ./<pkgdir>` |
| JS/TS | `.ts`/`.tsx`/`.mts`/`.cts`/`.js`/`.jsx`/`.mjs`/`.cjs` | `<runner> test` | `<runner> test -- <file>` | user-error (see below) |

JS runner is picked from the lockfile only: `pnpm-lock.yaml` -> `pnpm`,
`yarn.lock` -> `yarn`, `package-lock.json` / `npm-shrinkwrap.json` ->
`npm`. The lockfile search starts at the nearest `package.json`
above the file and walks upward **within the VC root**, so a
workspace at `packages/app/package.json` correctly resolves the
`pnpm-lock.yaml` at the monorepo root. Missing lockfile is a
`user-error`; the runner is never guessed from `package.json` alone.

`C-c t n` for JS/TS deliberately signals `user-error` rather than
picking a jest/vitest/mocha runner by heuristic. Use file tests or
set `my/workflow-nearest-test-command` for that buffer. The existing project
override remains project-only; file and nearest now have their own overrides.
JS file tests require a test script that accepts a file argument.

Filenames are passed through `shell-quote-argument`, so a file named
`weird name; rm -rf.py` is safe. Project roots and workspace names
that contain spaces are handled the same way.

Go file-scope runs the **enclosing package**, not an isolated-file
compile. This matches how `go test` is meant to be invoked.

Python nearest uses `python-info-current-defun` and preserves class names
in an exact pytest node ID. It refuses a non-test function. Go nearest
checks the nearest preceding function declaration; it refuses helpers
instead of accidentally selecting an earlier test, and uses `^TestX$`.

### Overriding the command

Each scope has its own buffer-local override variable. There is no
cascade: the file override does not affect project, the project
override does not affect file, and so on.

```elisp
(setq-local my/workflow-project-test-command  "make test-fast")
(setq-local my/workflow-file-test-command     "pytest -x tests/test_api.py")
(setq-local my/workflow-nearest-test-command  "pytest -k my_case")
```

Overrides are literal commands, not format strings. None of the three is
marked with `safe-local-variable`: a
`.dir-locals.el` or file-local assignment prompts on first read.
Free-form shell strings from an untrusted checkout stay unsafe by
default; do not add a blanket safe predicate. We deliberately do
**not** auto-trust a dir-local shell command.

### Shared project context

`modules/proj-context.el` is the small resolver behind test command
selection and Apheleia formatter dispatch. Given a file (or the
current buffer), `(my/proj-context)` returns a plist:

- `:file`             - absolute path or nil.
- `:language`         - `:python | :go | :ts | :js | :emacs-lisp | nil`.
- `:root`             - nearest language-specific manifest directory.
- `:vcs-root`         - root selected by `project-current`, which bounds
  every upward search.
- `:package-manager`  - `:pnpm | :yarn | :npm` (JS/TS only).
- `:node-modules`     - nearest `node_modules/` at or above `:root`.
- `:venv`             - `.venv/` at `:root` when it exists (Python only).

Rules:

- Language comes from the file's extension (or `major-mode` when the
  buffer has no file). It is never inferred from a project marker;
  the pre-round-2 bug picked `:python` from `pyproject.toml` for a
  TypeScript file, which routed `C-c t p` to `pytest` in a mixed
  repo. Tests do not run on save.
- Search paths are canonical, including symlink aliases. Searches stop at
  `:vcs-root`; an unrelated boundary raises an error before inspecting files.
  Without a project boundary, standalone searches can reach `/`.
- Unknown language returns `:language nil, :root nil`. Callers turn
  that into an actionable `user-error`; the resolver never guesses.
- `my/proj-context` never launches a formatter or language server.
  It reads file-existence metadata and calls `project-current`,
  which can invoke VC subprocesses. Emacs file handlers can use remote I/O.

## Save-time formatting (Apheleia)

`modules/development.el` configures **Apheleia as the sole save-time
formatter**, including for Go. `modules/lsp.el` deliberately does
**not** wire `lsp-format-buffer` or `lsp-organize-imports` onto
`before-save-hook`, to avoid formatting the same save twice.

- **Python** -> `ruff format`. Picked from `exec-path`, so an
  activated project venv (see `environments.el`) puts
  `.venv/bin/ruff` first automatically. The formatter context
  therefore respects the buffer's venv without any extra wiring.
- **Go** -> `my/go-format`, a named apheleia formatter that resolves
  to `goimports` when available (a superset of `gofmt` that also
  manages imports) and falls back to `gofmt`. Named to make the sole
  owner obvious in `apheleia-mode-alist`.
- **TypeScript / JavaScript / JSON** -> `prettier`. The command
  list uses a form (`my/apheleia-prettier-arg1`) so apheleia
  re-resolves the executable each save:

  1. Nearest `node_modules/.bin/prettier` at or above the file,
     bounded by the VC root. The search walks EVERY ancestor: a
     nested workspace whose `node_modules/` omits prettier does
     not block a hoisted root prettier from being picked up.
  2. `executable-find "prettier"` on `exec-path` (a globally
     installed binary is a safe local invocation).
  3. The literal string `prettier`, which triggers apheleia's own
     `executable-find` guard so the formatter skips cleanly. We
     also emit an install-locally hint chosen by the resolved
     package manager: `pnpm / yarn / npm add -D prettier`. We
     **never** fall through to `npx`; that could install a package
     on save.

  JSON, CSS, HTML, and Markdown are formatter *assets*, not test
  languages. `my/proj-context` returns `:language nil` for them so
  `C-c t` fails closed. The prettier resolver starts its search
  from the buffer's own directory instead, which keeps formatting
  behavior identical to source files while leaving the test-command
  dispatcher unpolluted.
- **Emacs Lisp** -> `lisp-indent` (built-in reindent).

We read the installed apheleia (`emacs/.emacs.d/elpa/apheleia-<v>/`)
to confirm two things before wiring the above:

- `apheleia-formatters` entries whose value is a list of
  strings/symbols support non-string elements: any form is
  `eval`'d at dispatch and its result (string or list of strings)
  is spliced into the argv. That is how `my/apheleia-prettier-arg1`
  and `my/apheleia-go-arg1` inject the buffer-relative executable.
- Apheleia never falls through to a download: `apheleia-npx` is a
  bundled shell script that resolves `node_modules/.bin/<cmd>` and
  execs the local binary, and apheleia's `executable-find` guard
  short-circuits when nothing is available. We do not use the `npx`
  symbol or the `apheleia-npx` shell shim in this configuration --
  the prettier resolver returns the concrete local binary path.

### Optional pilot install

The declared package set now includes `magit`, `embark`, and
`embark-consult`. To install them explicitly:

```
M-x my/install-packages
```

The rest of Emacs can start without them. Install the declared packages
before using Magit; Embark keys are only installed when its package exists.
Restart Emacs after installation. Remove the workflow module from the loader
to remove its keybindings; uninstalling packages alone leaves bound symbols.

## Tests and limits

Static Python tests (already existed):

```sh
python3 -m unittest discover -s scripts/tests -p 'test_*.py'
```

New ERT tests (batch, hermetic):

```sh
emacs -Q --batch -l scripts/tests/emacs-config.el \
      -f ert-run-tests-batch-and-exit
emacs -Q --batch -l scripts/tests/emacs-workflow.el \
      -f ert-run-tests-batch-and-exit
emacs -Q --batch -l scripts/tests/emacs-proj-context.el \
      -f ert-run-tests-batch-and-exit
```

The workflow suite stubs `compile` and uses temp project roots. It
never runs a real test suite, never mutates a real git repo, and never
starts a subprocess. It covers key wiring, per-language project/file
commands, JS lockfile selection, shell quoting of scary filenames, Go
package-dir (not file) semantics, per-project rerun isolation, missing
project / unsupported project / missing JS lockfile / JS nearest
`user-error` paths, buffer-local override precedence and its lack of
auto-safe marker, and Python/Go nearest anchor logic.

The proj-context suite (`emacs-proj-context.el`) additionally covers:

- Mixed Python + JS + Go + pnpm-lock repositories: a `.ts` file picks
  `:ts` and runs `pnpm test`, not `pytest`.
- Nested workspaces: `packages/app/package.json` with `pnpm-lock.yaml`
  at the monorepo root resolves to `:root = packages/app` with
  `:package-manager = :pnpm`, and file-scoped tests compile with
  `default-directory = packages/app`.
- Upward search boundary: a lockfile above the pinned VC root is
  invisible.
- Package-manager choice: `pnpm` / `yarn` / `npm` /
  `npm-shrinkwrap.json`.
- Paths with spaces at every level: workspace name, filename, and
  intermediate directory.
- Per-scope overrides: project, file, and nearest each apply only to
  their own command; none is marked `safe-local-variable`.
- Apheleia is the sole Go format owner: `lsp.el` no longer defines
  `lsp-go-install-save-hooks` nor adds `lsp-format-buffer` /
  `lsp-organize-imports` on any hook, and `go-mode-hook` is clean of
  those functions.
- Prettier resolves to project-local `node_modules/.bin/prettier`; a
  missing formatter surfaces an install-locally message (`pnpm /
  yarn / npm add -D prettier`) rather than falling through to `npx`.
- Fail-closed kind detection: a `.txt` buffer inside a Python repo
  does not silently become `:python` via marker inference.
- Scope-root consistency: project scope, file scope, nearest scope,
  and rerun all compile at the same resolved root; a nested
  workspace runs project tests at the inner package.json directory,
  and rerun after a file test finds the same entry.
- Hoisted prettier / hoisted `typescript-language-server`: a nested
  workspace whose `node_modules/` omits the tool still resolves to
  the hoisted root binary.
- JSON prettier resolution: a `.json` buffer picks the local
  prettier even though `my/proj-context` classifies it as a
  non-test language.
- Nested Python subproject: `my/venv--project-root-or-nil` resolves
  the inner `.venv` for a `.py` file under `services/api/`, not the
  outermost VC root.

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

Two full-init smoke checks are available: installed packages via `make smoke`,
and missing packages via `make test`.

1. **Installed-packages smoke.** Uses the developer's real MELPA cache
   under a temporary HOME, Custom file, and eln-cache. Rejects package
   installs and URL retrieval. Confirms the tracked configuration still
   loads with the packages that are actually present on this machine.

   ```sh
   emacs -Q --batch -l scripts/tests/emacs-smoke.el
   ```

   `make smoke` also runs a dedicated Apheleia dispatch integration
   smoke (`scripts/tests/emacs-apheleia-smoke.el`) that loads the
   real installed Apheleia and calls `apheleia--formatter-context`
   directly against the configured `apheleia-formatters` /
   `apheleia-mode-alist` entries. It asserts on the resolved `arg1`
   and `argv` slots for a TS buffer with a local
   `node_modules/.bin/prettier` and for a Go buffer with `goimports`
   on `exec-path`. `apheleia--execute-formatter-process` is stubbed
   to raise, so no real formatter runs. The suite prints `SKIP` and
   exits 0 when the Apheleia package is not installed, so a fresh
   machine still passes `make smoke`.

2. **Fresh-HOME smoke.** Points `package-user-dir` at an empty temporary
   directory, so no third-party package is installed. Loads `init.el`
   verbatim and then opens sample `.py`, `.el`, `.go`, `.ts`, and `.tsx`
   buffers. The mocks block `package-refresh-contents`,
   `package-install`, and `url-retrieve`; they deliberately do **not**
   stub `yas-minor-mode` or `lsp-deferred`. The language modules must
   guard those calls with `fboundp` themselves, and this test is what
   catches the regression if they stop.

   ```sh
   emacs -Q --batch -l scripts/tests/emacs-fresh-smoke.el
   ```

Neither check opens projects, launches servers, or contacts the network.
Customize writes go to a local `custom.el` inside the temporary HOME;
backup and autosave files go to ignored state directories. Autosave and
backups remain enabled. After installing packages or grammars
explicitly, restart Emacs to apply all conditional hooks and mode
mappings.

### Missing-dependency fallback

Each language module (`python.el`, `typescript.el`, `go.el`, `elisp.el`)
guards every optional dependency at call time:

- `yas-minor-mode` and `lsp-deferred` are wrapped in `(when (fboundp ...))`.
- `dap-mode`, `flycheck`, `lsp-pyright`, and other packages already
  used `package-installed-p` guards; those stay.
- `.go` maps to `go-mode` when its function or autoload is available,
  and to `prog-mode` otherwise. A `.go` file still opens without
  the `go-mode` package installed.
- `.ts` and `.tsx` fall through to their tree-sitter mode when the
  grammar is available, then to `typescript-mode` if that legacy
  package is installed, and finally to `prog-mode`. This is set from
  `modules/typescript.el` **after** `treesitter.el` runs, so a real
  grammar always wins.

These guards are exercised by the fresh-HOME smoke test; regressing them
surfaces as a failure in `make test`.

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
