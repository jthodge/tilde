# Terminal

## Prevent `complete:13: command not found: compdef`
## https://stackoverflow.com/questions/66338988/complete13-command-not-found-compdef
## TODO: resolve without this autoload
autoload -Uz compinit
compinit

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

## Spaceship Prompt (https://spaceship-prompt.sh/)
source /opt/homebrew/opt/spaceship/spaceship.zsh

## fzf  (https://github.com/junegunn/fzf)
source <(fzf --zsh)

## zoxide (https://github.com/ajeetdsouza/zoxide)
eval "$(zoxide init zsh)"

# Development Tooling

# C

# Compiler
alias gcc=/opt/homebrew/bin/gcc-14
# alias gcc=/usr/bin/gcc

# Haskell

## GHCup
[ -f "/Users/jth/.ghcup/env" ] && . "/Users/jth/.ghcup/env"

## JavaScript

### Volta
export VOLTA_HOME="${HOME}/.volta"
export PATH="${VOLTA_HOME}/bin:${PATH}"

## Python

### uv
eval "$(uv generate-shell-completion zsh)"

## Zig
export PATH=$PATH:/opt/homebrew/opt/zig

# Networking

## ngrok

if command -v ngrok > /dev/null; then
    eval "$(ngrok completion)"
fi

