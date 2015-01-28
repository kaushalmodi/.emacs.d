#!/bin/tcsh -f

set org_dirs = `find . -type d -name "org-2*"`

foreach dir (${org_dirs})
    cd ${dir}

    # Make backup of org files that need to be patched
    mv ox-latex.el ox-latex.elbkp

    # Remove the compiled versions of those files
    rm ox-latex.elc

    # Copy the patched version from backup
    \cp -f ${HOME}/.emacs.d/backup/ox-latex.el .

    cd -
end
