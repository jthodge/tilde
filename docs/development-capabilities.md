# Development capability checks

`make doctor` checks declared installation state. **`make capabilities` runs
installed tools** on synthetic files with temporary homes and caches. It is
explicit: not part of editor/shell startup, and not a CI requirement to install
all language tools. Its unit tests are part of `make verify`.

The probe command writes JSON to stdout, narration to stderr, and returns 1
when a required capability fails. `--only python|node|go` selects a group;
`--summary` suppresses JSON.

## Declared scope

| Group | Required functional probes | Optional |
| --- | --- | --- |
| Python | Interpreter version; one pytest test; exact Ruff formatting output; pyright-langserver LSP initialization | None |
| JS/TS | Node version and built-in test runner; strict `tsc --noEmit`; exact Prettier output; typescript-language-server LSP initialization | None |
| Go | Go version; stdlib-only `go test`; exact gofmt output; gopls LSP initialization | goimports adds missing standard-library imports |

There are **13 required probes and one optional probe**. Runtime versions are
reported, not changed. `scripts/runtime-versions.env` remains authoritative for
Node **22.14.0**, pnpm **10.15.0**, and yarn **4.4.0** defaults. An isolated Pi
runtime can put another Node version on the probe process's PATH.

LSP initialization must return a JSON-RPC 2.0 response for request 1 with a
`result.capabilities` object. This proves startup/protocol readiness, **not**
completion, diagnostics, project dependency loading, editor integration, or
performance on real projects. The JS test uses Node's built-in runner; it does
not validate a project's pnpm/yarn/npm test script.

## Interpreter and dependency ownership

Python selection is explicit `--python`, then `~/.venv/base/bin/python3`, then
the runner's interpreter. `--python` selects the interpreter/pytest environment;
formatter and server binaries still resolve through the caller's PATH.
Activating a shell environment does not override the base-venv preference.

For a Python project, declare its development dependencies and test its actual
interpreter, rather than adding packages to the shared base just to green a
probe:

```sh
cd /path/to/project
uv add --dev pytest ruff
uv sync
~/tilde/scripts/check-capabilities --only python --python "$PWD/.venv/bin/python3"
```

These are manual setup commands, not commands run by the probes. A project
without uv metadata needs its own explicit dependency setup. `make tools`
creates the optional base venv; it does not promise pytest in every environment.

For JS/TS, use the project's selected package manager, for example:

```sh
pnpm add -D typescript prettier typescript-language-server
```

Project binaries take precedence in Emacs through the shared project resolver.
The capability probe itself uses PATH; prepend a vetted project's
`node_modules/.bin` explicitly to test those installations. It never calls
`npx` or installs missing packages. Global Volta tools are a separate fallback,
not a replacement for project dependency locks.

Go comes from the existing Go installation; gopls and optional goimports are
separate tools. Review a version before installing, for example with
`go install golang.org/x/tools/cmd/goimports@VERSION`. Nothing installs merely
because goimports is absent: Emacs retains its gofmt fallback.

## Isolation and resource limits

- Temporary HOME, XDG directories, TMPDIR, Go caches, GOPATH and GOENV.
- Forwarded PATH/LANG and Volta installation location, but no credential
  variables, PYTHONPATH, NODE_OPTIONS or inherited Go overrides. Volta shims
  can read installation metadata and write their own logs; this exception is
  **not** enforced read-only.
- Go uses `GOPROXY=off`, `GOSUMDB=off`, `GOTOOLCHAIN=local`, and disabled telemetry.
- Pytest plugin autoload is disabled. Python package-index access is disabled.
- TypeScript automatic typing acquisition is disabled in initialization options.
- No real project content is passed to the tools. This is not a filesystem or
  network sandbox; installed tools and their launchers remain trusted.

Commands have 5–60 second deadlines; LSP initialization has 15 seconds, plus
bounded cleanup grace. Reader threads use readiness polling and capped buffers
(256 KiB per command stream; 2 MB LSP stdout, 128 KiB LSP stderr). Process groups
are signalled even if the parent has exited; processes that create their own
session are outside that group. Threads stop before pipes close.

The unit suite uses synthetic processes and mocked capability groups. It covers
malformed framing, timeouts, output floods, silent descendants retaining pipes,
missing tools, formatter-output expectations, reporting, and workspace cleanup.
It does not run installed language servers as an incidental unit-test step.

## Adoption result

The workstation run passed **12/13 required probes**. The selected base Python
reported `No module named pytest`; optional goimports was absent. These gaps
remain visible as a nonzero capability result. No dependency was installed to
hide them. Actual formatter and LSP initialization probes passed; real editor
completion, project tests, and debugger workflows remain separate checks.
