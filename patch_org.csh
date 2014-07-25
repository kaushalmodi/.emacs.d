#!/bin/tcsh -f

set org_dir = `find . -type d -name "org-2*"`

cd ${org_dir}
set org_dir = `pwd`

# Make backup of org files that need to be patched
mv ox-html.el ox-html.elbkp
mv ox-latex.el ox-latex.elbkp

# Remove the compiled versions of those files
rm ox-html.elc
rm ox-latex.elc

# Copy the patched version from backup
cp ~/.emacs.d/backup/ox-html.el .
cp ~/.emacs.d/backup/ox-latex.el .

cd -
