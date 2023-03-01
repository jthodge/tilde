# Set prompt
# Configuration located at tilde/zsh/.spaceshiprc.zsh
source "/usr/local/opt/spaceship/spaceship.zsh"

# Configure z navigation
# https://github.com/rupa/z
. ~/z/z.sh

# Remove duplicate history when reverse-searching commands
# TODO: relocate env vars to .zshenv
# TODO: relocate all zsh dotfiles to tilde/zsh/
HISTSIZE=5000
HISTFILE=~/.zsh_history
SAVEHIST=5000
HISTDUP=erase
setopt appendhistory
setopt sharehistory
setopt incappendhistory
setopt hist_ignore_all_dups
setopt hist_save_no_dups
setopt hist_ignore_dups
setopt hist_find_no_dups

# Enable case-insensitive command autocompletion
# https://superuser.com/questions/1092033/how-can-i-make-zsh-tab-completion-fix-capitalization-errors-for-directories-and
autoload -U compinit && compinit
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'

# Enable emacs-flavored keybindings
bindkey -e

# Add Homebrew sbin to PATH
export PATH="/usr/local/sbin:$PATH"

# Configure Go development
export GOPATH="${HOME}/golang"
export GOROOT="$(brew --prefix golang)/libexec"
export PATH="$PATH:${GOPATH}/bin:${GOROOT}/bin"
test -d "${GOPATH}" || mkdir "${GOPATH}"
test -d "${GOPATH}/src/github.com" || mkdir -p "${GOPATH}/src/github.com"

# Change window title
# https://scriptthe.net/2014/12/14/changing-your-iterm2-window-title/
# https://superuser.com/questions/292652/change-iterm2-window-and-tab-titles-in-zsh
# $1 = type; 0 - both, 1 - tab, 2 - title
# rest = text
setTerminalText () {
    # echo works in bash & zsh
    local mode=$1 ; shift
    echo -ne "\033]$mode;$@\007"
}
stt_both  () { setTerminalText 0 $@; }
stt_tab   () { setTerminalText 1 $@; }
stt_title () { setTerminalText 2 $@; }

# Configure Google Cloud
# Update PATH to include Google Cloud SDK
if [ -f '/Users/jth/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/jth/google-cloud-sdk/path.zsh.inc'; fi
# Enable shell command autocompletion for gcloud CLI
if [ -f '/Users/jth/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/jth/google-cloud-sdk/completion.zsh.inc'; fi

# Configure Ruby development
# Initialize rbenv
# https://github.com/rbenv/rbenv/issues/815#issuecomment-152975421
eval "$(rbenv init -)"

# Configure Node development
# Increase amount of "old space" node allocates to combat large build sizes
export NODE_OPTIONS=--max_old_space_size=8192
# Configure Volta
# https://docs.volta.sh/guide/getting-started
export VOLTA_HOME="$HOME/.volta"
export PATH="$VOLTA_HOME/bin:$PATH"
# Configure bun
[ -s "/Users/th/.bun/_bun" ] && source "/Users/th/.bun/_bun"
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"

# Configure Rust development
export PATH="$HOME/.cargo/bin:$PATH"

# Decompress files that have been compressed with zlib's DEFLATE
alias inflate='ruby -r zlib -e "STDOUT.write Zlib::Inflate.inflate(STDIN.read)"'

# Configure Java development
export PATH="$HOME/.jenv/bin:$PATH"
eval "$(jenv init -)"

# Configure thefuck
# https://github.com/nvbn/thefuck#installation
eval $(thefuck --alias)

# Configure mcfly
# https://github.com/cantino/mcfly#installation
eval "$(mcfly init zsh)"

# Configure Python development
# Configure pyenv
export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
# Configure Poetry
export PATH="$HOME/.poetry/bin:$PATH"

# TODO: figure out if these are load-bearing
export PATH="$PATH:$HOME/.local/bin"
[[ -e ~/.profile ]] && emulate sh -c 'source ~/.profile'

# Configure OCaml development
# Configure opam
[[ ! -r /Users/th/.opam/opam-init/init.zsh ]] || source /Users/th/.opam/opam-init/init.zsh  > /dev/null 2> /dev/null

# Enable shell output coloring to colorize:
# ls
# man
export TERM="xterm-256color" CLICOLOR=1
export LESS_TERMCAP_mb=$(print -P "%F{cyan}") \
    LESS_TERMCAP_md=$(print -P "%B%F{red}") \
    LESS_TERMCAP_me=$(print -P "%f%b") \
    LESS_TERMCAP_so=$(print -P "%K{magenta}") \
    LESS_TERMCAP_se=$(print -P "%K{black}") \
    LESS_TERMCAP_us=$(print -P "%U%F{green}") \
    LESS_TERMCAP_ue=$(print -P "%f%u")

# Enable zsh-syntax-highlighting plugin
# N.B. _THIS MUST BE SOURCED AT THE END OF .zshrc
# https://github.com/zsh-users/zsh-syntax-highlighting/tree/master
source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
