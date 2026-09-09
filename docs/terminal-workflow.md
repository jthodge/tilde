# Terminal workflow

The tracked workflow is Ghostty, fish, tmux, sesh, and Neovim. Ghostty is
the sole supported terminal; Alacritty and iTerm2 are deprecated and removed
from the package manifests. The retired Alacritty Stow package is removed.

Ghostty explicitly launches `/opt/homebrew/bin/fish -l`, independent of the
macOS account shell. Both Option keys act as Alt, so Alt-h/j/k/l crosses
editor and tmux panes. Ghostty uses Berkeley Mono 10pt, 14px padding,
no window decorations, and a `#121212` background matching tmux's status bar.
The Fish-native prompt matches Mark Tran's: one line, directory plus Git state, no frames
or segment backgrounds. See [prompt and awareness](prompt-and-awareness.md).

## Selected changes

- Neovim `<leader>E` opens the file explorer without also being a prefix.
- `<leader>qq` saves and quits all buffers. It replaces `<leader>Eq`.
- `<leader>tt` opens the Snacks theme picker.
- Relative line numbers, picker previews, fonts, themes, and `lazy-lock.json`
  are unchanged. These are personal preferences, not correctness fixes.
- tmux sets `XDG_CONFIG_HOME` for new panes and clears `default-command`
  **after** plugins load, so a plugin cannot replace fish with a stale shell.
- Ghostty's default `xterm-ghostty` TERM matches tmux's built-in xterm
  clipboard declarations. Only `tmux*` needs an extra declaration, not
  every terminal. Reloading clears the retired terminal's override.

`tmux-sensible` already provides the desired escape delay, focus events, and
scrollback size. There is no need to duplicate those settings.

## Verification

```sh
nvim --headless --clean -l scripts/tests/nvim.lua
PYTHONDONTWRITEBYTECODE=1 python3 -m unittest discover -s scripts/tests -p test_tmux.py
/Applications/Ghostty.app/Contents/MacOS/ghostty +validate-config
```

After changing the config, reload Ghostty with Cmd+Shift+, or restart it.
Use new terminal surfaces to verify shell changes. Inside tmux, prefix + r
reloads its config without replacing existing pane processes. Pin Ghostty
in the Dock manually; old terminal preferences are not deleted by a normal
Homebrew uninstall (do not use `--zap` unless intentionally purging them).

The Neovim check parses Lua and inspects key declarations with plugin stubs.
The tmux test runs a private server with a stub plugin that attempts to change
the shell command. It uses `sleep`, not a login shell, and kills only that
private server afterward.

These checks do not validate a real SSH clipboard round trip or visual picker
behavior. On a trusted remote host, confirm a Neovim yank reaches the local
clipboard before relying on it. Existing panes keep their running shells;
use a new pane to check a new shell selection.
