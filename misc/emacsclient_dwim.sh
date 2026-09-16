#!/usr/bin/env bash
# Time-stamp: <2021-07-09 10:26:39 kmodi>

# Open emacsclient with a new frame only if one does not exist.
# http://emacs.stackexchange.com/a/12897/115

# Usage: Alias this script to something like 'e' in the shell.
#        Then if emacs server hasn't yet been started, run "e &". After that
#        open files using "e <FILE(s)>&".

# Example:
# > e &         # Fresh start of emacs; launches emacsclient, starts server,
#               # loads files from my saved desktop, etc.
# > e foo.txt & # Opens foo.txt in the already opened emacsclient frame.

debug=${EMACSCLIENT_DWIM_DEBUG:-0}

opts=()

# Did the user explicitly ask for a terminal frame?
tty_requested=0
# .. or explicitly ask for a new frame?
frame_requested=0
for arg in "$@"
do
    case "$arg" in
        -nw|-t|--tty)
            tty_requested=1
            ;;
        -c|--create-frame)
            frame_requested=1
            ;;
    esac
done

# Works for both X11 ($DISPLAY) and Wayland/pgtk ($WAYLAND_DISPLAY) builds.
if [[ ${tty_requested} -eq 0 ]] && [[ -n "$DISPLAY" || -n "$WAYLAND_DISPLAY" ]]
then
    # Is there any graphical frame already open?
    frame=$(emacsclient -a '' \
                        -e '(if (seq-some (function display-graphic-p) (frame-list)) t nil)' \
                        2>/dev/null)

    if [[ ${debug} -eq 1 ]]
    then
        echo "dbg: DISPLAY=$DISPLAY WAYLAND_DISPLAY=$WAYLAND_DISPLAY"
        echo "dbg: graphical frame present = $frame"
        echo "dbg: terminal names = $(emacsclient -a '' -e '(mapcar (function terminal-name) (frame-list))' 2>/dev/null)"
    fi

    # If there is no graphical frame open, create one.
    if [[ "$frame" != "t" && ${frame_requested} -eq 0 ]]
    then
        opts+=('-c')
    fi

    # Don't block the shell.
    opts+=('-n')
elif [[ ${tty_requested} -eq 0 ]]
then
    # No graphical display available, so open emacsclient in the terminal.
    opts+=('-nw')
fi

if [[ ${debug} -eq 1 ]]
then
    echo "dbg: emacsclient -a '' ${opts[*]} $*"
fi

exec emacsclient -a '' "${opts[@]}" "$@"
