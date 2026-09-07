# Mark-style abbreviations for the high-frequency commands
abbr -a b brew
abbr -a g git
abbr -a n nvim
abbr -a s sesh
abbr -a t tmux
abbr -a vi nvim

# Modern CLI replacements

## bat (https://github.com/sharkdp/bat)
alias cat=bat

## eza (https://github.com/eza-community/eza)
alias ls=eza

## fd (https://github.com/sharkdp/fd)
alias find=fd

## procs (https://github.com/dalance/procs)
alias ps=procs

## dust (https://github.com/bootandy/dust)
alias du=dust

# Development tooling

## gcc-14 pin
alias gcc=/opt/homebrew/bin/gcc-14

## uv-backed pip
alias pip="uv pip"

# Agents

## Claude — defined as a function (functions/claude.fish) so it can
## lazy-load HONCHO_API_KEY from 1Password before launch, matching the
## honcho() lazy-credential pattern.
