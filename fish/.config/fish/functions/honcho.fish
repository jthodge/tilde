function honcho --description "Honcho CLI with lazy credential load"
    test -z "$HONCHO_API_KEY"; and honcho_load
    command honcho $argv
end
