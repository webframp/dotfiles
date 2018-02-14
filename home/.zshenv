# This file is always sourced
# anything needed in PATH goes here
typeset -U path
path=($HOME/.local/bin
      $HOME/.cargo/bin
      $path[@])

#autoload -U compaudit compinit
#compinit

# Chef
if [[ -x "/usr/bin/chef" ]]; then
    eval "$(chef shell-init zsh)"
fi

export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
