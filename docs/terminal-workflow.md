# Terminal workflow

The tracked workflow remains Alacritty (primary), Ghostty (fallback), fish,
tmux, sesh, and Neovim. Alt-h/j/k/l still crosses editor and tmux panes.

## Selected changes

- Neovim `<leader>E` opens the file explorer without also being a prefix.
- `<leader>qq` saves and quits all buffers. It replaces `<leader>Eq`.
- `<leader>tt` opens the Snacks theme picker.
- Relative line numbers, picker previews, fonts, themes, and `lazy-lock.json`
  are unchanged. These are personal preferences, not correctness fixes.
- tmux sets `XDG_CONFIG_HOME` for new panes and clears `default-command`
  **after** plugins load, so a plugin cannot replace fish with a stale shell.
- Clipboard features extend tmux's built-in xterm declarations only for
  `tmux*` and `alacritty*`, not every terminal.

`tmux-sensible` already provides the desired escape delay, focus events, and
scrollback size. There is no need to duplicate those settings.

## Verification

```sh
nvim --headless --clean -l scripts/tests/nvim.lua
PYTHONDONTWRITEBYTECODE=1 python3 -m unittest discover -s scripts/tests -p test_tmux.py
```

The Neovim check parses Lua and inspects key declarations with plugin stubs.
The tmux test runs a private server with a stub plugin that attempts to change
the shell command. It uses `sleep`, not a login shell, and kills only that
private server afterward.

These checks do not validate a real SSH clipboard round trip or visual picker
behavior. On a trusted remote host, confirm a Neovim yank reaches the local
clipboard before relying on it. Existing panes keep their running shells;
use a new pane to check a new shell selection.
