function __uv_autovenv
    test -n "$VIRTUAL_ENV"; and return
    test -d "$HOME/.venv/$UV_DEFAULT_VENV"; or uv venv "$HOME/.venv/$UV_DEFAULT_VENV"
    source "$HOME/.venv/$UV_DEFAULT_VENV/bin/activate.fish"
end
