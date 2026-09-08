# Identity

## 1Password SSH agent
export SSH_AUTH_SOCK="${HOME}/Library/Group Containers/2BUA8C4S2C.com.1password/t/agent.sock"

# JavaScript

## Volta
export VOLTA_HOME="${HOME}/.volta"
export PATH="${VOLTA_HOME}/bin:${PATH}"
export VOLTA_FEATURE_PNPM=1

# Python

## uv
export PATH="${HOME}/.local/bin:$PATH"

# Rust — cargo env is optional. Fresh checkouts without rustup installed
# would previously fail login with `no such file or directory`.
[ -r "$HOME/.cargo/env" ] && . "$HOME/.cargo/env"
