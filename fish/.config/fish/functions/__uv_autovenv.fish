function __uv_autovenv --description "Source an existing base venv activation, if configured. Never creates."
    # Opt-out.
    test "$TILDE_AUTO_VENV" = 0; and return 0
    # Preserve any active virtualenv the caller already has.
    test -n "$VIRTUAL_ENV"; and return 0
    # Require a configured venv name.
    test -n "$UV_DEFAULT_VENV"; or return 0

    set -l activate "$HOME/.venv/$UV_DEFAULT_VENV/bin/activate.fish"
    # Source only an already-existing activation script. Never invoke uv;
    # never create directories. Base venv provisioning is an explicit,
    # out-of-band step.
    test -f "$activate"; or return 0
    source "$activate"
end
