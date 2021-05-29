# Exit
die() {
  echo "$1" >&2
  exit "${2-1}"
}

# nerd-font patched (complete) font required! See
# https://github.com/ryanoasis/nerd-fonts
# http://nerdfonts.com/#cheat-sheet
__info(){ printf "\e[0;32m\uF00C $@\e[0m\n"; }  # 
__error(){ printf "\e[0;31m\uF00D $@\e[0m\n"; } # 
__warn(){ printf "\e[0;33m\uF128 $@\e[0m\n"; }  # 

_exists() { (( $+commands[$1] )) }
