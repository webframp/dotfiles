# This file is always sourced
# PROFILE_STARTUP=true
# if [[ "$PROFILE_STARTUP" == true ]]; then
#     # http://zsh.sourceforge.net/Doc/Release/Prompt-Expansion.html
#     PS4=$'%D{%M%S%.} %N:%i> '
#     exec 3>&2 2>$HOME/tmp/startlog.$$
#     setopt xtrace prompt_subst
# fi
# anything needed in PATH goes here
typeset -gU cdpath fpath path manpath path
path=($HOME/.local/bin
      $HOME/.cargo/bin
      $HOME/bin
      $HOME/go/bin
      $path[@])

# Chef
chefinit=$HOME/.chef-init
if [[ -r "$chefinit" ]]; then
    autoload -Uz compinit && compinit
    source $chefinit
else
    echo "Chef init not loaded, run 'chef shell-init zsh > $chefinit'"
fi

export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
export GOPATH=$HOME/go

export HOMEBREW_NO_INSECURE_REDIRECT=1
export HOMEBREW_CASK_OPTS=--require-sha
