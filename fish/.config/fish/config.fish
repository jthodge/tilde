# Brew must come first — every `type -q` check below depends on its PATH
/opt/homebrew/bin/brew shellenv fish | source

source $__fish_config_dir/exports.fish
source $__fish_config_dir/aliases.fish
source $__fish_config_dir/colors.fish

test -f $HOME/.cargo/env.fish; and source $HOME/.cargo/env.fish

type -q mise;    and mise activate fish | source
type -q zoxide;  and zoxide init --cmd j fish | source
type -q fzf;     and fzf --fish | source
type -q uv;      and uv generate-shell-completion fish | source
type -q ngrok;   and ngrok completion --fish 2>/dev/null | source

__uv_autovenv

test -f $__fish_config_dir/local.fish; and source $__fish_config_dir/local.fish
