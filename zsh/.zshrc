# Legacy zsh — fish is the primary interactive shell (see AGENTS.md).
# Kept working so a direct `zsh` invocation stays usable, but without
# any startup-time secret fetches or environment provisioning.

# Terminal completion
autoload -Uz compinit
compinit

# Aliases — modern CLI replacements
alias cat=bat
alias ls=eza
alias find=fd
alias ps=procs
alias du=dust

# Development tooling
alias gcc=/opt/homebrew/bin/gcc-14
alias pip="uv pip"

# Prompt
[ -r /opt/homebrew/opt/spaceship/spaceship.zsh ] \
    && source /opt/homebrew/opt/spaceship/spaceship.zsh

# Integrations — all guarded by command availability so a missing tool
# never breaks startup.
command -v fzf    >/dev/null 2>&1 && source <(fzf --zsh)
command -v zoxide >/dev/null 2>&1 && eval "$(zoxide init zsh)"
command -v uv     >/dev/null 2>&1 && eval "$(uv generate-shell-completion zsh)"
command -v mise   >/dev/null 2>&1 && eval "$(mise activate zsh)"
command -v ngrok  >/dev/null 2>&1 && eval "$(ngrok completion)"

# Legacy language toolchains — sourced only if still installed.
[ -f "$HOME/.ghcup/env" ] && . "$HOME/.ghcup/env"
[ -r "$HOME/.opam/opam-init/init.zsh" ] \
    && source "$HOME/.opam/opam-init/init.zsh" > /dev/null 2>&1

# Python — activate an existing base venv only. Never create.
export UV_DEFAULT_VENV="${UV_DEFAULT_VENV:-base}"
if [ "${TILDE_AUTO_VENV:-1}" != 0 ] && [ -z "$VIRTUAL_ENV" ] && [ -f "$HOME/.venv/$UV_DEFAULT_VENV/bin/activate" ]; then
    source "$HOME/.venv/$UV_DEFAULT_VENV/bin/activate"
fi

# activate — validate the target *before* deactivating anything, so a
# typo or a missing venv path does not leave the shell without its
# previously-active environment. `deactivate` runs only after the new
# activation script has been confirmed readable.
activate() {
    if [ "$#" -ne 1 ] || [ -z "$1" ]; then
        print -u2 "activate: usage: activate <venv-path>"
        return 2
    fi
    if [ ! -f "$1/bin/activate" ] || [ ! -r "$1/bin/activate" ]; then
        print -u2 "activate: no activation script at $1/bin/activate"
        return 1
    fi
    [ -n "$VIRTUAL_ENV" ] && deactivate
    # shellcheck disable=SC1091
    source "$1/bin/activate"
}

# Go — g-install manages this line; do not edit. See https://github.com/stefanmaric/g
export GOPATH="$HOME/go"; export GOROOT="$HOME/.go"; export PATH="$GOPATH/bin:$PATH"; # g-install: do NOT edit, see https://github.com/stefanmaric/g

# Agents — lazy credential load.
#
# HONCHO_API_KEY is not exported at startup: `op read` is slow, prompts on
# a locked vault, and leaks the secret to every subprocess. Fetch it just
# in time when honcho / claude are actually invoked.
export HONCHO_PEER_NAME="taylor"

honcho_load() {
    export HONCHO_API_KEY="$(op read 'op://Personal/Honcho/credential' 2>/dev/null)"
}

honcho() (
    [ -z "$HONCHO_API_KEY" ] && honcho_load
    command honcho "$@"
)

claude() (
    [ -z "$HONCHO_API_KEY" ] && honcho_load
    command claude "$@"
)
