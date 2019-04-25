## Profiling
#zmodload zsh/zprof
# Entirety of my startup file...
source $HOME/.zsh/prompt.zsh
source $HOME/.zsh/config.zsh

# lazy load any custom functions
lazyload_fpath=$HOME/.zsh/autoload
fpath=($lazyload_fpath $fpath)
if [[ -d "$lazyload_fpath" ]]; then
    for func in $lazyload_fpath/*; do
        autoload -Uz ${func:t}
    done
fi
unset lazyload_fpath

# completion
# -i means silently ignore insecure files
autoload -Uz compinit
typeset -i updated_at=$(date +'%j' -r ~/.zcompdump 2>/dev/null || stat -f '%Sm' -t '%j' ~/.zcompdump 2>/dev/null)
if [ $(date +'%j') != $updated_at ]; then
    compinit -i
else
    compinit -C -i
fi

# menu select
unsetopt menu_complete

# completion performance improvements
# Force prefix matching, avoid partial globbing on path
zstyle ':completion:*' accept-exact '*(N)'
# enable completion cache
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.local/share/zsh/cache

# Ignore completion for non-existent commands
zstyle ':completion:*:functions' ignored-patterns '(_*|pre(cmd|exec))'
#zstyle ':completion:*:functions' ignored-patterns '_*'


# completion behavior adjustments
# Case insensitive, partial-word and substring competion
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|=*' 'l:|=* r:|=*'
# zstyle ':completion:*:*:*:*:*' menu select
# zstyle ':completion:*' special-dirs true

# # Colors in the completion list
# zstyle ':completion:*' list-colors ''
# zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'

# from: vault -autocomplete-install
# autoload -U +X bashcompinit && bashcompinit
# complete -o nospace -C /usr/local/bin/vault vault

# Aliases
alias reload!='exec "$SHELL" -l'
# http://zsh.sourceforge.net/Doc/Release/Expansion.html#Parameter-Expansion
(( $+commands[exa] )) && {
    alias l='exa'
    alias la='exa -la'
    alias ll='exa -lag'
    alias lg='exa -bghHliS --git'
}
alias alacritty='/Applications/Alacritty.app/Contents/MacOS/alacritty'

# Git
alias gl='git pull --prune'
alias gd='git diff'
alias gco='git checkout'
alias gpo='git push origin HEAD'
alias gp='git push origin HEAD'
alias gpa='git push origin --all'
alias gst='git status'
alias gs='git status -sb'
alias gcmh='git commit --amend -C HEAD'

# Docker
alias dockerhostshell='docker run -it --privileged --pid=host debian nsenter -t 1 -m -u -n -i sh'

# Kubernetes
alias k='kubectl'
compdef k=kubectl

# TODO members, groups, vault, Golang, Rust
alias cat='bat --plain'

# Homebrew
alias brews='brew list -1'
alias bubo='brew update && brew outdated'
alias bubc='brew upgrade && brew cleanup'
alias bubu='bubo && bubc'

# Ruby
alias be='bundle exec'

# Addons
if test -d "/usr/local/opt/homeshick"; then
    export HOMESHICK_DIR=/usr/local/opt/homeshick
    source "/usr/local/opt/homeshick/homeshick.sh"
    alias h='homeshick'
elif test -d "$HOME/.homesick/repos/homeshick"; then
    source "$HOME/.homesick/repos/homeshick/homeshick.sh"
    alias h='homeshick'
fi

(( $+commands[direnv] )) && {
    _direnv_hook() {
        eval "$("/usr/local/bin/direnv" export zsh)";
    }
    typeset -ag precmd_functions;
    if [[ -z ${precmd_functions[(r)_direnv_hook]} ]]; then
        precmd_functions+=_direnv_hook;
    fi
}
# only export if sekey exists
(( $+commands[sekey] )) && {
    export SSH_AUTH_SOCK=$HOME/.sekey/ssh-agent.ssh
}

source $HOME/.zsh/correction.zsh
source $HOME/.zsh/dots.zsh

if test -f "$HOME/.zsh/local.zsh"; then
    source $HOME/.zsh/local.zsh
fi

# Load colors from less, and others.
[[ -f ~/.LESS_TERMCAP ]] && . ~/.LESS_TERMCAP

unset updated_at

autoload -U select-word-style
select-word-style bash

export ZSH_AUTOSUGGEST_USE_ASYNC=1
## Source plugins last
# static method, after updates run:
# antibody bundle <~/.zsh_plugins.txt > ~/.zsh_plugins.sh
source ~/.zsh_plugins.sh

bindkey '^Xr' zaw-history
bindkey '^ ' autosuggest-accept
