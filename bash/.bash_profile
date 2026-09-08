# Legacy bash — kept working for the rare direct `bash -l` invocation.
# fish is the primary interactive shell and zsh is the account fallback.
# Nothing here contacts the network, fetches a secret, or requires an
# installed language toolchain to succeed.

# --- PATH -------------------------------------------------------------

# Prefer Homebrew (Apple Silicon first, then Intel fallback) so /opt or
# /usr/local tools override the system defaults. `~/bin` stays highest.
if [ -d /opt/homebrew/bin ]; then
    PATH="/opt/homebrew/bin:/opt/homebrew/sbin:$PATH"
elif [ -d /usr/local/bin ]; then
    PATH="/usr/local/bin:/usr/local/sbin:$PATH"
fi
PATH="$HOME/bin:$PATH"
export PATH

# --- editor -----------------------------------------------------------

export EDITOR="vim"

# --- optional git helpers --------------------------------------------

# `~/.git-completion.bash` and `~/.git-prompt.sh` are not standard on a
# fresh macOS install: they live under `git/contrib/completion` in
# git upstream and have to be dropped in place manually. Source them
# only when present so a clean HOME still gives us a usable prompt.
[ -r "$HOME/.git-completion.bash" ] && . "$HOME/.git-completion.bash"

green="\[\033[0;32m\]"
blue="\[\033[0;34m\]"
purple="\[\033[0;35m\]"
reset="\[\033[0m\]"

if [ -r "$HOME/.git-prompt.sh" ]; then
    # shellcheck disable=SC1091
    . "$HOME/.git-prompt.sh"
fi
if declare -F __git_ps1 >/dev/null; then
    export GIT_PS1_SHOWDIRTYSTATE=1
    # '\u' current user, '\W' basename of cwd, '\$(__git_ps1)' git status.
    PS1="$purple\\u$green\$(__git_ps1)$blue \\W \$ $reset"
else
    # __git_ps1 is unavailable; fall back to a plain prompt so bash does
    # not print "command not found: __git_ps1" on every keystroke.
    PS1="$purple\\u$blue \\W \$ $reset"
fi
export PS1

# --- Go ---------------------------------------------------------------

export GOPATH="$HOME/go"
export PATH="$PATH:$GOPATH/bin"

# --- JavaScript -------------------------------------------------------

# Volta is authoritative for Node on this machine. The previous NVM
# bootstrap alongside Volta introduced a second runtime owner. Do not restore it
# without also removing Volta.
export VOLTA_HOME="$HOME/.volta"
export PATH="$VOLTA_HOME/bin:$PATH"

# --- local user bins --------------------------------------------------

export PATH="$PATH:$HOME/.local/bin:$HOME/.poetry/bin"
