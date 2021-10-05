# =============================================================================
#
# Utility functions for zoxide.
#

# pwd based on the value of _ZO_RESOLVE_SYMLINKS.
function __zoxide_pwd() {
    \builtin pwd -L
}

# cd + custom logic based on the value of _ZO_ECHO.
function __zoxide_cd() {
    # shellcheck disable=SC2164
    \builtin cd "$@"
}

# =============================================================================
#
# Hook configuration for zoxide.
#

# Hook to add new entries to the database.
function __zoxide_hook() {
    zoxide add -- "$(__zoxide_pwd)"
}

# Initialize hook.
# shellcheck disable=SC2154
if [[ ${precmd_functions[(Ie)__zoxide_hook]} -eq 0 ]] && [[ ${chpwd_functions[(Ie)__zoxide_hook]} -eq 0 ]]; then
    chpwd_functions=("${chpwd_functions[@]}" "__zoxide_hook")
fi

# =============================================================================
#
# When using zoxide with --no-aliases, alias these internal functions as
# desired.
#

# Jump to a directory using only keywords.
function __zoxide_z() {
    if [ "$#" -eq 0 ]; then
        __zoxide_cd ~
    elif [ "$#" -eq 1 ] && [ "$1" = '-' ]; then
        if [ -n "${OLDPWD}" ]; then
            __zoxide_cd "${OLDPWD}"
        else
            # shellcheck disable=SC2016
            \builtin printf 'zoxide: $OLDPWD is not set'
            return 1
        fi
    elif [ "$#" -eq 1 ] && [ -d "$1" ]; then
        __zoxide_cd "$1"
    else
        \builtin local result
        result="$(zoxide query --exclude "$(__zoxide_pwd)" -- "$@")" \
            && __zoxide_cd "${result}"
    fi
}

# Jump to a directory using interactive search.
function __zoxide_zi() {
    \builtin local result
    result="$(zoxide query -i -- "$@")" && __zoxide_cd "${result}"
}

# =============================================================================
#
# Convenient aliases for zoxide. Disable these using --no-aliases.
#

# Remove definitions.
function __zoxide_unset() {
    \builtin unalias "$@" &>/dev/null
    \builtin unfunction "$@" &>/dev/null
    \builtin unset "$@" &>/dev/null
}

__zoxide_unset 'z'
function z() {
    __zoxide_z "$@"
}

__zoxide_unset 'zi'
function zi() {
    __zoxide_zi "$@"
}

# =============================================================================
#
# To initialize zoxide, add this to your configuration (usually ~/.zshrc):
#
# eval "$(zoxide init zsh)"
