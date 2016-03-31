#!/bin/env tcsh

cd src

touch Makefile
touch toolbox/Makefile
touch templates/Makefile

make

mv *.elc ../.
\cp -rf *.el ../.
\cp -rf templates ../.
\cp -rf toolbox ../.

cd ..
