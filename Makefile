DIR = $(shell basename `pwd`)
TIMESTAMP = $(shell date | tr ' :' '__')

tar:
	tar cvfz emacsd_$(TIMESTAMP).tar.gz -h *.el *.elc setup-files matlab-emacs python-mode.el-6.1.1 snippets from-git plantuml ditaa

clean:
	find . -name "*.*~" | xargs \rm -f

dellast:
	find . -type f -name "emacsd*.gz" -printf '%T@ %p\n' | sort -n | tail -1 | cut -f2- -d" " | xargs rm
