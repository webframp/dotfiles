# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
if test -f "/usr/bin/sw_vers"; then
    export ZSH=/Users/sme/.oh-my-zsh
else
    export ZSH=/home/sme/.oh-my-zsh
fi

function is_macbook_pro() {
    local pro=$(system_profiler SPHardwareDataType|grep -c MacBookPro13,3)
    if test $pro -eq 1; then
        return 0
    fi
    return 1
}

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
# could be conditional on powerline fonts installed
ZSH_THEME="agnoster"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git z aws vault kops kubectl rust)

if test ! -f "/usr/bin/sw_vers"; then
    plugins+=archlinux
fi

if test ! is_macbook_pro; then
    plugins+=ssh-agent
fi

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/rsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Aliases
alias h='homeshick'
alias y='yaourt'
alias p='pacman'
alias x='ssh-agent startx'

if test -x "$(which exa)"; then
    alias ls='exa'
    alias ll='exa -lg'
    alias lg='exa -bghHliS'
fi

# Git
alias gpo='git push origin HEAD'
alias gst='git status'

# Golang
# Rust

# Homebrew
alias brews='brew list -1'
alias bubo='brew update && brew outdated'
alias bubc='brew upgrade && brew cleanup'
alias bubu='bubo && bubc'

# Addons
if test -d "/usr/local/opt/homeshick"; then
    export HOMESHICK_DIR=/usr/local/opt/homeshick
    source "/usr/local/opt/homeshick/homeshick.sh"
fi
if [ $commands[direnv] ]; then
    source <(direnv hook zsh)
fi
if [ $commands[chef] ]; then
    source <(chef shell-init zsh)
fi
if [ $commands[minikube] ]; then
    source <(minikube completion zsh)
fi

# ZSH and OMZ Plugin config
zstyle ':completion:*' rehash true

if test ! is_macbook_pro; then
    zstyle :omz:plugins:ssh-agent identities id_rsa
fi

# only export if running latest MacBookPro w/Secure Enclave
if is_macbook_pro; then
    export SSH_AUTH_SOCK=$HOME/.sekey/ssh-agent.ssh
fi

# Source custom functions, longer stuff goes here
for ZFILE in $HOME/.zsh/*; do
    source $ZFILE
done
