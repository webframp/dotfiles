# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="gallifrey"

# Set to this to use case-sensitive completion
CASE_SENSITIVE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(knife git github rvm extract ssh-agent gem autojump vagrant)

source $ZSH/oh-my-zsh.sh

# Customize to your needs..
HOSTNAME=`hostname`

#RVM
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" 

# ALIASES
alias l='ls -F -h'
alias ll='ls -l -F -h'
alias la='ls -a -F -h'
alias lla='ls -l -a -F -h'

# command equivalents
alias -g A='|ack'
alias -g L='|less'
alias -g S='&> /dev/null &'
alias -g G='|grep'
alias -g GC='|grep --color=always'

if [ `hostname | grep -c soundwave` -eq 1 -a ! `uname` = "Linux" ]; then
    # handy, but only needed osx my workstation
    alias startwin='VBoxManage startvm Win7'
    alias stopwin='VBoxManage controlvm Win7 savestate'

    if [ -f /Applications/Emacs.app/Contents/MacOS/Emacs ]; then
        alias emacs=/Applications/Emacs.app/Contents/MacOS/Emacs
        alias emacsclient=/Applications/Emacs.app/Contents/MacOS/bin/emacsclient
        export EDITOR="/Applications/Emacs.app/Contents/MacOS/Emacs -nw"
    else
        export EDITOR="emacs -nw"
    fi

    # suffix aliases on the mac
    alias -s flac=open
    alias -s mp3=open
    alias -s ogg=open
    alias -s wav=open
    alias -s avi=open
    alias -s mpeg=open
    alias -s mpg=open
    alias -s wmv=open
    alias -s bmp=open
    alias -s png=open
    alias -s jpg=open
    alias -s jpeg=open
    alias -s gif=open
    alias -s pdf=open
    alias -s pyc=python

fi # end mac only settings

if [ -f $HOME/.zsh/functions ]; then
    source $HOME/.zsh/functions
fi

# nocorrect aliases
NOCORRECT=~/.zsh/nocorrect
if [ -f $NOCORRECT ]; then
    for p in `cat $NOCORRECT`; do alias $p="nocorrect $p"; done
fi

# drop into tmux if this is a login shell and we're not in tmux
# already
#    if (which tmux >/dev/null) && [[ -o login ]] &&  [[ -z "$TMUX" ]] && [[ -f $HOME/bin/tmuxsh ]] && [[ $ITERM_PROFILE != 'Hotkey Window' ]]; then
#        exec zsh "$HOME/bin/tmuxsh" $(which tmux)
#    fi

# some random stuff
alias rb19='rvm use 1.9.3'
alias x='ssh-agent startx'
alias lsg='git status'
alias vi=ec # kill old habits
alias irssi="if [[ -n $TMUX ]]; then TERM=screen-256color irssi; else irssi; fi"


# config editing
alias notes='$EDITOR ~/notes.org'
alias work='$EDITOR ~/org/work.org'
alias muttrc='$EDITOR ~/.muttrc'
alias zshrc='$EDITOR ~/.zshrc'
alias vimrc='$EDITOR ~/.vimrc'
alias hgrc='$EDITOR ~/.hgrc'
alias emacsrc='$EDITOR ~/.emacs.d/init.el'
alias xinitrc='$EDITOR ~/.xinitrc'

# xmonad related
alias editmonad='$EDITOR ~/.xmonad/xmonad.hs'
alias testmonad='ghci ~/.xmonad/xmonad.hs'

# may not work right on Lion
alias -s org=$BROWSER
alias -s com=$BROWSER
alias -s net=$BROWSER
alias -s html=$BROWSER
# for archlinux
alias -s PKGBUILD=$EDITOR

# setup env
setopt ALL_EXPORT
HISTFILE=${HOME}/.zhistory
HISTSIZE=3000 # big ol' hist file
SAVEHIST=3000
DIRSTACKSIZE=20 # rarely need more than that
BROWSER="/usr/bin/conkeror"
PAGER=less
LESS='-RMS'
LESS_TERMCAP_mb=$'\E[01;31m'
LESS_TERMCAP_md=$'\E[01;31m'
LESS_TERMCAP_me=$'\E[0m'
LESS_TERMCAP_se=$'\E[0m'
LESS_TERMCAP_so=$'\E[01;44;33m'
LESS_TERMCAP_ue=$'\E[0m'
LESS_TERMCAP_us=$'\E[01;32m'
EDITOR=ec
GREP_COLOR='1;32' #bright green
GREP_OPTIONS='--color=auto'
TEMP="/tmp"
PYTHONSTARTUP="$HOME/.pythonrc"
PYTHONPATH=$PYTHONPATH:.
PATH=~/bin:/usr/local/bin:/usr/texbin:$HOME/.rvm/bin:/bin:/usr/sbin:/sbin:/usr/bin:/usr/X11/bin
ALLUSERSPROFILE="$HOME/.pkg/"
# put private stuff in a separate, git ignored file
source $HOME/.zsh/private_env
unsetopt ALL_EXPORT

# for ssh-agent plugin
zstyle :omz:plugins:ssh-agent agent-forwarding on
zstyle :omz:plugins:ssh-agent id_rsa id_rsa_estately

# user functions moved to ~/.zsh/functions
# no correct commands in ~/.zsh/nocorrect

