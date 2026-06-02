function activate --argument-names venv
    test -n "$VIRTUAL_ENV"; and deactivate
    source "$venv/bin/activate.fish"
end
