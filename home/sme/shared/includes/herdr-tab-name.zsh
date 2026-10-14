# ABOUTME: Names herdr tabs after the command running in them, and "zsh" at an idle prompt
# ABOUTME: Sourced by modules/home-manager/herdr.nix; does nothing outside herdr

if [[ -n $HERDR_PANE_ID ]]; then
  zmodload zsh/datetime

  # Labels this shell set, newest last: the launcher replaces only these or
  # herdr's default number. A microsecond timestamp orders the background
  # renames, so a slow one can't overwrite a newer one.
  typeset -ga _herdr_tab_known

  _herdr_tab_set() {
    [[ $1 == ${_herdr_tab_known[-1]-} ]] && return
    local seq
    printf -v seq '%.0f' $(( EPOCHREALTIME * 1e6 ))
    @herdr_launch@ tab-name "$seq" "$1" "${_herdr_tab_known[@]}" &>/dev/null &!
    _herdr_tab_known=("${(@)_herdr_tab_known[-2,-1]}" "$1")
  }

  # Name after the first real word: skip env assignments and precommand modifiers.
  _herdr_tab_preexec() {
    local w
    for w in ${(z)1}; do
      case $w in
        *=* | sudo | exec | command | builtin | nocorrect | noglob | time | nohup) continue ;;
      esac
      break
    done
    [[ -n $w ]] && _herdr_tab_set ${${(Q)w}:t}
  }

  _herdr_tab_precmd() {
    _herdr_tab_set zsh
  }

  autoload -Uz add-zsh-hook
  add-zsh-hook preexec _herdr_tab_preexec
  add-zsh-hook precmd _herdr_tab_precmd
fi
