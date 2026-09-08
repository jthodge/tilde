# Homebrew (Apple Silicon) — mirror `brew shellenv`'s essential exports
# without spawning brew on every login shell. If /opt/homebrew is absent,
# this block is skipped. Trailing colons retain default manual/info paths.
if [ -d /opt/homebrew/bin ]; then
    export HOMEBREW_PREFIX="/opt/homebrew"
    export HOMEBREW_CELLAR="/opt/homebrew/Cellar"
    export HOMEBREW_REPOSITORY="/opt/homebrew"
    export PATH="/opt/homebrew/bin:/opt/homebrew/sbin${PATH:+:$PATH}"
    export MANPATH="/opt/homebrew/share/man:${MANPATH-}"
    export INFOPATH="/opt/homebrew/share/info:${INFOPATH-}"
fi
