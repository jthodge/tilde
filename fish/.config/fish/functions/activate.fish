function activate --argument-names venv --description "Activate a Python venv; validate before touching current state."
    # Validate arguments and the target activation script *before*
    # deactivating anything. A typo or missing path used to leave the
    # shell with no active environment because we deactivated first.
    if test (count $argv) -ne 1; or test -z "$venv"
        echo "activate: usage: activate <venv-path>" >&2
        return 2
    end
    if not test -f "$venv/bin/activate.fish"; or not test -r "$venv/bin/activate.fish"
        echo "activate: no activation script at $venv/bin/activate.fish" >&2
        return 1
    end
    test -n "$VIRTUAL_ENV"; and deactivate
    source "$venv/bin/activate.fish"
end
