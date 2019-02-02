#!/bin/zsh
function e() { emacsclient -n $@ }
function ec()  { emacs -nw $@ }
function ed() { emacs --daemon }
function et() { emacsclient -t $@ }
function es() { e --daemon=$1 && ec -s $1 }
function el() { ps ax | egrep '[Ee]macs' }
function ek() { ec -e '(kill-emacs)' -s $1 }

# Simple calculator
calc() {
    local result=""
    result="$(printf "scale=10;%s\\n" "$*" | bc --mathlib | tr -d '\\\n')"
    #						└─ default (when `--mathlib` is used) is 20

    if [[ "$result" == *.* ]]; then
        # improve the output for decimal numbers
        # add "0" for cases like ".5"
        # add "0" for cases like "-.5"
        # remove trailing zeros
        printf "%s" "$result" |
            sed -e 's/^\./0./'  \
                -e 's/^-\./-0./' \
                -e 's/0*$//;s/\.$//'
    else
        printf "%s" "$result"
    fi
    printf "\\n"
}

# Determine size of a file or total size of a directory
fs() {
    if du -b /dev/null > /dev/null 2>&1; then
        local arg=-sbh
    else
        local arg=-sh
    fi
    # shellcheck disable=SC2199
    if [[ -n "$@" ]]; then
        du $arg -- "$@"
    else
        du $arg -- .[^.]* *
    fi
}

# Run `dig` and display the most useful info
digga() {
    dig +nocmd "$1" any +multiline +noall +answer
}

# Query Wikipedia via console over DNS
mwiki() {
    dig +short txt "$*".wp.dg.cx
}

# Show all the names (CNs and SANs) listed in the SSL certificate
# for a given domain
getcertnames() {
    if [ -z "${1}" ]; then
        echo "ERROR: No domain specified."
        return 1
    fi

    local domain="${1}"
    echo "Testing ${domain}…"
    echo ""; # newline

    local tmp
    tmp=$(echo -e "GET / HTTP/1.0\\nEOT" \
              | openssl s_client -connect "${domain}:443" 2>&1)

    if [[ "${tmp}" = *"-----BEGIN CERTIFICATE-----"* ]]; then
        local certText
        certText=$(echo "${tmp}" \
                       | openssl x509 -text -certopt "no_header, no_serial, no_version, \
      no_signame, no_validity, no_issuer, no_pubkey, no_sigdump, no_aux")
        echo "Common Name:"
        echo ""; # newline
        echo "${certText}" | grep "Subject:" | sed -e "s/^.*CN=//"
        echo ""; # newline
        echo "Subject Alternative Name(s):"
        echo ""; # newline
        echo "${certText}" | grep -A 1 "Subject Alternative Name:" \
            | sed -e "2s/DNS://g" -e "s/ //g" | tr "," "\\n" | tail -n +2
        return 0
    else
        echo "ERROR: Certificate not found."
        return 1
    fi
}

# Call from a local repo to open the repository on gitlab/github/bitbucket in browser
# Modified version of https://github.com/zeke/ghwd
repo() {
  # Figure out github repo base URL
  local base_url
  base_url=$(git config --get remote.origin.url)
  base_url=${base_url%\.git} # remove .git from end of string

  # Fix git@github.com: URLs
  base_url=${base_url//git@github\.com:/https:\/\/github\.com\/}

  # Fix git://github.com URLS
  base_url=${base_url//git:\/\/github\.com/https:\/\/github\.com\/}

  # Fix git@bitbucket.org: URLs
  base_url=${base_url//git@bitbucket.org:/https:\/\/bitbucket\.org\/}

  # Fix git@gitlab.com: URLs
  base_url=${base_url//git@gitlab\.com:/https:\/\/gitlab\.com\/}

  # Fix git@appsgit.bethel.jw.org: URLs
  base_url=${base_url//git@appsgit\.bethel\.jw\.org:/https:\/\/appsgit\.bethel\.jw\.org\/}

  # Validate that this folder is a git folder
  if ! git branch 2>/dev/null 1>&2 ; then
    echo "Not a git repo!"
    exit $?
  fi

  # Find current directory relative to .git parent
  full_path=$(pwd)
  git_base_path=$(cd "./$(git rev-parse --show-cdup)" || exit 1; pwd)
  relative_path=${full_path#$git_base_path} # remove leading git_base_path from working directory

  # If filename argument is present, append it
  if [ "$1" ]; then
    relative_path="$relative_path/$1"
  fi

  # Figure out current git branch
  # git_where=$(command git symbolic-ref -q HEAD || command git name-rev --name-only --no-undefined --always HEAD) 2>/dev/null
  git_where=$(command git name-rev --name-only --no-undefined --always HEAD) 2>/dev/null

  # Remove cruft from branchname
  branch=${git_where#refs\/heads\/}

  [[ $base_url == *bitbucket* ]] && tree="src" || tree="tree"
  url="$base_url/$tree/$branch$relative_path"


  echo "Calling $(type open) for $url"

  open "$url" &> /dev/null || (echo "Using $(type open) to open URL failed." && exit 1);
}

# `tre` is a shorthand for `tree` with hidden files and color enabled, ignoring
# the `.git` directory, listing directories first. The output gets piped into
# `less` with options to preserve color and line numbers, unless the output is
# small enough for one screen.
tre() {
    tree -aC -I '.git' --dirsfirst "$@" | less -FRNX
}

# vault integration for my aws creds

lskbsecure(){
    ioreg -l -w 0 \
        | perl -nle 'print $1 if /"kCGSSessionSecureInputPID"=(\d+)/' \
        | uniq \
        | xargs -I{} ps -p {} -o comm=
}

# Possible prompt additions
function prompt_zsh_spotifyStatus () {
    local color='%F{white}'
    SPOTIFY_ICON=$'\uf1bc' # 
    state=`osascript -e 'tell application "Spotify" to player state as string'`;
    if [ $state = "playing" ]; then
        artist=`osascript -e 'tell application "Spotify" to artist of current track as string'`;
        track=`osascript -e 'tell application "Spotify" to name of current track as string'`;

        echo -n "%{$color%}   $artist - $track " ;

    fi
}
