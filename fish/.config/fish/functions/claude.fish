function claude --description "Claude Code with lazy Honcho credential load"
    # The Honcho memory plugin reads HONCHO_API_KEY from the environment at
    # launch (src/config.ts: process.env.HONCHO_API_KEY || config.apiKey).
    # Interactive shells stay fast by not eagerly running `op read`; load the
    # credential just-in-time here so each Claude Code session inherits it.
    test -z "$HONCHO_API_KEY"; and honcho_load
    command /Users/jth/.local/bin/claude $argv
end
