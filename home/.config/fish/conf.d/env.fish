set IS_MACBOOK_PRO (system_profiler SPHardwareDataType | grep -c MacBookPro)
set LESSPIPE (which src-hilite-lesspipe.sh)
set -x LESSOPEN "| $LESSPIPE %s"
set -x LESS "--RAW-CONTROL-CHARS -X -F"

if test $IS_MACBOOK_PRO -eq 1
    set -x SSH_AUTH_SOCK "$HOME/.sekey/ssh-agent.ssh"
end

if test -f $HOME/.LESS_TERMCAP
    set -x LESS_TERMCAP_mb (tput bold; tput setaf 2) # green
    set -x LESS_TERMCAP_md (tput bold; tput setaf 6) # cyan
    set -x LESS_TERMCAP_me (tput sgr0)
    set -x LESS_TERMCAP_so (tput bold; tput setaf 3; tput setab 4) # yellow on blue
    set -x LESS_TERMCAP_se (tput rmso; tput sgr0)
    set -x LESS_TERMCAP_us (tput smul; tput bold; tput setaf 7) # white
    set -x LESS_TERMCAP_ue (tput rmul; tput sgr0)
    set -x LESS_TERMCAP_mr (tput rev)
    set -x LESS_TERMCAP_mh (tput dim)
    set -x LESS_TERMCAP_ZN (tput ssubm)
    set -x LESS_TERMCAP_ZV (tput rsubm)
    set -x LESS_TERMCAP_ZO (tput ssupm)
    set -x LESS_TERMCAP_ZW (tput rsupm)
end

set -x RUST_SRC_PATH (~/.cargo/bin/rustc --print sysroot)/lib/rustlib/src/rust/src
set -x GOPATH (/usr/local/bin/go env GOPATH)
