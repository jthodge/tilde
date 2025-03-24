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

## C

### Compiler
alias gcc=/opt/homebrew/bin/gcc-14
# alias gcc=/usr/bin/gcc

## Haskell

### GHCup
[ -f "/Users/jth/.ghcup/env" ] && . "/Users/jth/.ghcup/env"

## Python

### uv
eval "$(uv generate-shell-completion zsh)"

# Ensures:
# 1. A default venv (`base`) is always active if no other venv is active
# 2. `pip` is aliased to `uv pip`
# 3. Util function `activate` is provided to deactivate an existing venv before
# activating a new venv
export UV_DEFAULT_VENV="base"
if [ -z "$VIRTUAL_ENV" ]; then  # Only activate if no venv is currently active
    if [ ! -d "$HOME/.venv/$UV_DEFAULT_VENV" ]; then
        uv venv "$HOME/.venv/$UV_DEFAULT_VENV"
    fi
    source "$HOME/.venv/$UV_DEFAULT_VENV/bin/activate"
fi

alias pip="uv pip"

function activate() {
    if [ -n "$VIRTUAL_ENV" ]; then
        deactivate
    fi
    source "$1/bin/activate"
}

# Networking

## ngrok

if command -v ngrok > /dev/null; then
    eval "$(ngrok completion)"
fi

