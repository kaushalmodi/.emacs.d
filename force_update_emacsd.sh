#!/bin/sh
# Time-stamp: <2016-11-02 13:11:30 kmodi>

# Usage  : ./force_update_emacsd.sh <YOUR-EMACSD-PATH>
# Example: ./force_update_emacsd.sh ~/.emacs.d

# <YOUR-EMACSD-PATH>, if already existing, will be copied to the below backup dir
emacs_autobkp_dir="~/.emacs.d.bkpauto"

if [[ "$0" == "sh" ]] # called by source (source foo.sh)
then
    # http://stackoverflow.com/a/8912075/1219634
    script_file_name=$(readlink -f "${BASH_SOURCE[0]}")
else # called by direct execution of the script (./foo.sh)
    script_file_name=$(readlink -f "$0")
fi
script_dir=$(dirname "${script_file_name}")
# echo "Script file name: ${script_file_name}"
# echo "Script dir: ${script_dir}"

if [[ -z "$1" ]] # If the first argument is missing or an empty string
then
    echo "ERROR: Missing a mandatory argument."
    echo "  source $0 <YOUR-EMACSD-PATH>"
    echo "  Example: source $0 ~/.emacs.d"
    exit 1
else
    emacs_config_dir="$1"
fi

echo "The emacs config will be saved to ${emacs_config_dir}"
echo "Press Ctrl-C within 10 seconds if you don't want that .."
sleep 10
echo ''

start_dir=$(pwd)

if [[ ! -d "${emacs_config_dir}" ]]
then
    echo "Cloning .emacs.d to ${emacs_config_dir} .."
    git clone http://www.github.com/kaushalmodi/.emacs.d ${emacs_config_dir}
else
    cd ${emacs_config_dir}
    git_remote_origin_url=$(git config --get remote.origin.url)
    git_remote_origin_url_check=$(echo "${git_remote_origin_url}" | \sed 's/.*\(github\.com.*\.emacs\.d\).*/\1/')
    # Ensure that the we are not overriding a different git managed dir
    if [[ ${git_remote_origin_url_check} == "github.com/kaushalmodi/.emacs.d" ]]
    then
        if [[ -d "${emacs_autobkp_dir}" ]]
        then
            \rm -rf ${emacs_autobkp_dir} # Delete old backup if present
        fi
        echo "Backing up existing ${emacs_config_dir} to ${emacs_autobkp_dir} .."
        \cp -rf ${emacs_config_dir} ${emacs_autobkp_dir}

        echo "Force updating .emacs.d to ${emacs_config_dir} .."
        # Below will overwrite ALL GIT-MANAGED files
        # Files created locally/not git managed will remain untouched
        git fetch --all
        git reset --hard origin/master
    else
        echo "ERROR: The remote URL for this git repo is '${git_remote_origin_url}'."
        echo "Please try again using a different directory argument for downloading this .emacs.d."
        exit 1
    fi
fi
echo ''

cd ${emacs_config_dir}
git submodule init
git submodule update --force

cd ${start_dir}

# Self-destruct if the script is present in a path beginning with "/tmp"
if [[ ${script_dir} == /tmp* ]]
then
    # echo "Self-deleting ${script_file_name} ..\n"
    \rm -f ${script_file_name}
fi

echo "Done!!"
