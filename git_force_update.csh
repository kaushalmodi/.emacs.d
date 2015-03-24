#!/bin/tcsh -f
# Usage: source git_force_update.csh <YOUR .emacs.d PATH>
# Example: source git_force_update.csh ~/.emacs.d

if ( $#argv < 1 ) then
    echo "ERROR: Missing a mandatory argument."
    echo "  source git_force_update.csh <YOUR .emacs.d PATH>"
    echo "  Example: source git_force_update.csh ~/.emacs.d"
    exit 0
else
    set emacs_config_dir = $argv[1]
endif

echo "The emacs config will be saved to ${emacs_config_dir}."
echo "Press Ctrl-C within 10 seconds if you don't want that .."
sleep 10

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

if ( ! -d ${emacs_config_dir}/elisp ) then
    \mkdir -p ${emacs_config_dir}/elisp
endif

################################################################################
# List of package forks to be updated from http://www.github.com/kaushalmodi
set git_submodules = ( git-link interleave org-tree-slide rpn-calc smyx unfill zenburn-emacs zop-to-char )
foreach pkg (${git_submodules})
    set pkg_dir = "${emacs_config_dir}/elisp/${pkg}"
    if ( ! -d ${pkg_dir}/.git ) then
        if ( -d ${pkg_dir} ) then
            \rm -rf ${pkg_dir}
        endif
        echo "Cloning package ${pkg} to ${pkg_dir} .."
        git clone http://www.github.com/kaushalmodi/${pkg} ${pkg_dir}
    else
        echo "Force updating ${pkg} to ${pkg_dir} .."
        cd ${pkg_dir}; gfu
    endif
    echo ''
end

################################################################################
# Other git clones

# transpose-frame
set pkg_dir = "${emacs_config_dir}/elisp/transpose-frame"
if ( ! -d ${pkg_dir}/.git ) then
    if ( -d ${pkg_dir} ) then
        \rm -rf ${pkg_dir}
    endif
    echo "Cloning package transpose-frame to ${pkg_dir} .."
    git clone http://github.com/emacsmirror/transpose-frame ${pkg_dir}
else
    echo "Force updating transpose-frame to ${pkg_dir} .."
    cd ${pkg_dir}; gfu
endif
echo ''

# reveal.js
set pkg_dir = "${emacs_config_dir}/software/reveal.js"
if ( ! -d ${pkg_dir}/.git ) then
    if ( -d ${pkg_dir} ) then
        \rm -rf ${pkg_dir}
    endif
    echo "Cloning reveal.js to ${pkg_dir} .."
    git clone http://github.com/hakimel/reveal.js ${pkg_dir}
else
    echo "Force updating reveal.js to ${pkg_dir} .."
    cd ${pkg_dir}; gfu
endif
echo ''

cd ${start_dir}

unalias gfu
unset ${git_submodules}
unset ${pkg_dir}
unset ${emacs_autobkp_dir}
unset ${emacs_config_dir}
unset ${start_dir}

echo "Done\!\!"
