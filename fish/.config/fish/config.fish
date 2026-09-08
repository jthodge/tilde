# Fish startup — explicit phase order.
#
# Phase 1: environment (always)         — exports + safe PATH additions.
# Phase 2: local overrides (always)     — per-machine local.fish, before
#                                         any activation so it can retune
#                                         UV_DEFAULT_VENV / TILDE_AUTO_VENV.
# Phase 3: interactive-only             — colors, aliases, completions,
#                                         and tool integrations.
# Phase 4: interactive base venv        — source an existing activation
#                                         script only; never create.
#
# Non-interactive startup (scripts, `fish -c ...`) must not spawn brew,
# op, uv, or completion subprocesses. See docs/shell-startup.md.

# --- Phase 1: environment (always) ---

# Add Homebrew locations to PATH on macOS without spawning brew. Preserves
# existing PATH; `fish_add_path -gP` is idempotent and prepends.
if test -d /opt/homebrew/bin
    fish_add_path -gP /opt/homebrew/bin /opt/homebrew/sbin
end

source $__fish_config_dir/exports.fish

# --- Phase 2: local overrides (always, before activation) ---

test -f $__fish_config_dir/local.fish; and source $__fish_config_dir/local.fish

# --- Phase 3: interactive-only ---

if status is-interactive
    source $__fish_config_dir/aliases.fish
    source $__fish_config_dir/colors.fish

    test -f $HOME/.cargo/env.fish; and source $HOME/.cargo/env.fish

    type -q mise;   and mise activate fish | source
    type -q zoxide; and zoxide init --cmd j fish | source
    type -q fzf;    and fzf --fish | source
    type -q uv;     and uv generate-shell-completion fish | source
    type -q ngrok;  and ngrok completion --fish 2>/dev/null | source
end

# --- Phase 4: optional interactive existing-base-venv activation ---

if status is-interactive
    __uv_autovenv
end
