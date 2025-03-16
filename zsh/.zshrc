# Terminal

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

### Pyenv
export PYENV_ROOT="${HOME}/.pyenv"
export PYENV_VIRTUALENVWRAPPER_PREFER_PYVENV="true"
if command -v pyenv > /dev/null; then
  eval "$(pyenv init --path)";
  eval "$(pyenv init -)";
  pyenv virtualenvwrapper_lazy;
fi

## Zig
export PATH=$PATH:/opt/homebrew/opt/zig

# Networking

## ngrok

# 2024-08-30: ngrok autocompletion returns errors:
# (eval):2: command not found: compdef
# (eval):213: command not found: compdef

# if command -v ngrok > /dev/null; then
#   eval "$(ngrok completion)"
# fi

## zoxide (https://github.com/ajeetdsouza/zoxide)
eval "$(zoxide init zsh)"
