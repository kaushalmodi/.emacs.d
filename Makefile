DIR = $(shell basename `pwd`)
TIMESTAMP = $(shell date | tr ' :' '__')

tar:
	tar cvfz emacsd_$(TIMESTAMP).tar.gz -h *.el *.csh *.org setup-files snippets elisp software org-sty backup .gitignore .gitmodules .mc-lists.el

clean:
	find . -name "*.*~" | xargs \rm -f
	find . -name "#*.*#" | xargs \rm -f

dellast:
	find . -type f -name "emacsd*.gz" -printf '%T@ %p\n' | sort -n | tail -1 | cut -f2- -d" " | xargs rm
