typeset -U path
path=(~/.local/bin $path[@])

autoload -U compaudit compinit
compinit

# Chef
if [[ -x "/usr/bin/chef" ]]; then
    eval "$(chef shell-init zsh)"
fi
