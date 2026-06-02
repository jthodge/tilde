# Identity

## 1Password SSH agent
set -gx SSH_AUTH_SOCK "$HOME/Library/Group Containers/2BUA8C4S2C.com.1password/t/agent.sock"

# JavaScript

## Volta
set -gx VOLTA_HOME $HOME/.volta
set -gx VOLTA_FEATURE_PNPM 1
fish_add_path -p $VOLTA_HOME/bin

# Python

## uv
fish_add_path -p $HOME/.local/bin
set -gx UV_DEFAULT_VENV base

# Go
set -gx GOPATH $HOME/go
set -gx GOROOT $HOME/.go
fish_add_path $GOPATH/bin

# Honcho
set -gx HONCHO_PEER_NAME taylor
