#!/usr/bin/env bash
# Time-stamp: <2016-11-28 12:11:13 kmodi>

# Open emacsclient with a new frame only if one does not exist.
# http://emacs.stackexchange.com/a/12897/115

# Usage: Alias this script to something like 'e' in the shell.
#        Then if emacs server hasn't yet been started, run "e &". After that
#        open files using "e <FILE(s)>&".

# Example:
# > e &         # Fresh start of emacs; launches emacsclient, starts server,
#               # loads files from my saved desktop, etc.
# > e foo.txt & # Opens foo.txt in the already opened emacsclient frame.

if [[ "$DISPLAY" ]]
then
    frame=$(emacsclient -a '' \
                        -e "(member \"$DISPLAY\" (mapcar 'terminal-name (frames-on-display-list)))" \
                        2>/dev/null)

    # If there is no frame open create one
    if [[ "$frame" == "nil" ]]
    then
        opts='-c'
    fi

    # Don't wait (append the -n option) only if the user hasn't specified to
    # run in the terminal (-nw option).
    # http://www.tldp.org/LDP/abs/html/parameter-substitution.html
    # ${var#Pattern} Remove from $var the shortest part of $Pattern that matches
    # the front end of $var.
    # So if "$@" is "-nw foo.txt", ${@/#-nw/} will be just " foo.txt"
    if [[ "${@/#-nw/}" == "$@" ]]
    then
        opts="$opts -n"
    fi
else
    # Otherwise, open emacsclient in the terminal
    opts='-nw'
fi

exec emacsclient -a '' $opts "$@"
