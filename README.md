# tilde

Personal macOS dotfiles, deployed via [GNU Stow][stow].

## Bootstrap

```sh
git clone git@github.com:jthodge/tilde.git ~/tilde
cd ~/tilde
brew bundle install
stow -d . -t "$HOME" $(cat .stow-packages)
```

Each entry in `.stow-packages` (`bash`, `bin`, `emacs`, `git`, `zsh`)
mirrors a slice of `$HOME`. The `scripts/` directory is intentionally
not stowed — its contents are invoked in place.

## References

* [Managing dotfiles with GNU stow][stow]
* [Restore, Clone or Backup your Homebrew Setup](https://tomlankhorst.nl/brew-bundle-restore-backup/)
* [`Save As -> .pdf` Keyboard Shortcut](https://www.macsparky.com/blog/2008/3/19/keyboard-shortcut-for-save-as-pdf-in-os-x.html)
* [Universal Monitor Window Switcher](https://gist.github.com/jthodge/c4ba15a78fb29671dfa072fe279355f0)

[stow]: https://alexpearce.me/2016/02/managing-dotfiles-with-stow/
