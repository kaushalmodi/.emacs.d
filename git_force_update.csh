#!/bin/tcsh -f
# Time-stamp: <2015-10-21 13:55:51 kmodi>

# Usage: source git_force_update.csh <YOUR .emacs.d PATH>
# Example: source git_force_update.csh ~/.emacs.d

set called=($_)

if ( "${called}" != "" ) then  # called by source
   set script_file_name=`readlink -f $called[2]`
else # called by direct execution of the script
   set script_file_name=`readlink -f $0`
endif
unset {called}
set script_dir=`dirname ${script_file_name}`
# echo "Script file name: ${script_file_name}"
# echo "Script dir: ${script_dir}"

if ( $#argv < 1 ) then
    echo "ERROR: Missing a mandatory argument."
    echo "  source git_force_update.csh <YOUR .emacs.d PATH>"
    echo "  Example: source git_force_update.csh ~/.emacs.d"
    exit 0
else
    set emacs_config_dir = $argv[1]
endif

echo "The emacs config will be saved to ${emacs_config_dir}"
echo "Press Ctrl-C within 10 seconds if you don't want that .."
sleep 10
echo ''

set start_dir = `pwd`

set emacs_autobkp_dir = "~/.emacs.d.bkpauto"

# FORCE update
# Below alias will overwrite ALL GIT-MANAGED files
# Files created locally/not git managed will remain untouched
alias gfu 'git fetch --all; git reset --hard origin/master;'

if ( ! -d ${emacs_config_dir} ) then
    echo "Cloning .emacs.d to ${emacs_config_dir} .."
    git clone http://www.github.com/kaushalmodi/.emacs.d ${emacs_config_dir}
else
    if ( -d ${emacs_autobkp_dir} ) then
        \rm -rf ${emacs_autobkp_dir} # Delete old backup if present
    endif
    echo "Backing up existing ${emacs_config_dir} to ${emacs_autobkp_dir} .."
    \cp -rf ${emacs_config_dir} ${emacs_autobkp_dir}
    echo "Force updating .emacs.d to ${emacs_config_dir} .."
    cd ${emacs_config_dir}; gfu
endif
echo ''

cd ${emacs_config_dir}
git submodule init
git submodule update --force

cd ${start_dir}

# Self-destruct
if ( ${script_dir} == "/tmp" ) then
    # echo "Self-deleting ${script_file_name} ..\n"
    \rm -f ${script_file_name}
endif

unalias gfu
unset {emacs_autobkp_dir}
unset {emacs_config_dir}
unset {start_dir}
unset {script_file_name}
unset {script_dir}

echo "Done\!\!"
