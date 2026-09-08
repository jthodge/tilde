# Emacs workflow

## Explicit modules

`emacs/.emacs.d/init.el` loads modules in a fixed order. It does not scan a
directory, so adding a file does not silently change startup behavior.

- `bootstrap`: process paths and helper functions.
- `interface`: display, editing defaults, initial minibuffer setup.
- `packages`: package declarations and bootstrap.
- `lsp`: performance settings and shared language-server configuration.
- `treesitter`: grammar definitions and mode mapping.
- `development`: completion, formatters, snippets and debugger defaults.
- `bindings`: global command bindings.
- `typescript`, `python`, `go`, `elisp`: language-specific setup.
- `environments`: Python environment helpers.
- `custom-settings`: previously embedded Customize output.

## Mechanical extraction record

The initial split added a lexical-binding header to each module and changed
only the loader structure and separator whitespace. Removing the added
headers and restoring one separator newline after each module reproduced
the original 832-line `init.el` byte-for-byte:

```text
SHA256 9a497547c0068f6b3a9c67dc0bea8b55377c33eb284b5d2d1fc98e85de87b624
```

This verifies the extraction, not every possible effect of file boundaries.
The explicit loader is the intended difference. Subsequent behavior changes
must be separately tested and committed; do not treat this hash as their
expected final state.
