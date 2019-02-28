#
## Execute command at login before zshrc
#
if [[ -z "$LANG" ]]; then
    export LANG='en_US.UTF-8'
    export LANGUAGE=en_US.UTF-8
fi

export LC_COLLATE=en_US.UTF-8
export LC_CTYPE=en_US.UTF-8
export LC_MESSAGES=en_US.UTF-8
export LC_MONETARY=en_US.UTF-8
export LC_NUMERIC=en_US.UTF-8
export LC_TIME=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export LESSCHARSET=utf-8

#export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
export GOPATH=$HOME/go
export HOMEBREW_NO_INSECURE_REDIRECT=1
export HOMEBREW_CASK_OPTS=--require-sha

# Eliminate duplicates in *paths
typeset -gU cdpath fpath path manpath path

# Setup path
path=($HOME/.local/bin
      $HOME/.cargo/bin
      $HOME/bin
      $HOME/go/bin
      $path[@])

export PATH=/usr/local/sbin:/usr/local/bin:$PATH
