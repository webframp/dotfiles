# Entirety of my startup file...
source $HOME/.zsh/prompt.zsh
source $HOME/.zsh/config.zsh

# completion
autoload -Uz compinit
typeset -i updated_at=$(date +'%j' -r ~/.zcompdump 2>/dev/null || stat -f '%Sm' -t '%j' ~/.zcompdump 2>/dev/null)
if [ $(date +'%j') != $updated_at ]; then
    compinit -i
else
    compinit -C -i
fi

# from: vault -autocomplete-install
autoload -U +X bashcompinit && bashcompinit
complete -o nospace -C /usr/local/bin/vault vault

# Aliases
alias reload!='exec "$SHELL" -l'
# http://zsh.sourceforge.net/Doc/Release/Expansion.html#Parameter-Expansion
(( $+commands[exa] )) && {
    alias l='exa'
    alias la='exa -la'
    alias ll='exa -lag'
    alias lg='exa -bghHliS --git'
}

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

gi() {
    curl -s "https://www.gitignore.io/api/$*"
}

# Docker
alias dockerhostshell='docker run -it --privileged --pid=host debian nsenter -t 1 -m -u -n -i sh'

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
fi

(( $+commands[direnv] )) && {
    source <(direnv hook zsh)
}
# only export if sekey exists
(( $+commands[sekey] )) && {
    export SSH_AUTH_SOCK=$HOME/.sekey/ssh-agent.ssh
}

source $HOME/.zsh/functions.zsh
source $HOME/.zsh/correction.zsh
source $HOME/.zsh/dots.zsh
source $HOME/.zsh/local.zsh

# Load colors from less, and others.
[[ -f ~/.LESS_TERMCAP ]] && . ~/.LESS_TERMCAP

unset  updated_at

autoload -U select-word-style
select-word-style bash

## Source plugins last
# static method, after updates run:
# antibody bundle <~/.zsh_plugins.txt > ~/.zsh_plugins.sh
source ~/.zsh_plugins.sh
