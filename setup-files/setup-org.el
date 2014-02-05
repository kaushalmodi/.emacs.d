;; Time-stamp: <2014-02-05 17:31:29 kmodi>

;; Org Mode

(setq org-directory "~/org")

(setq org-src-fontify-natively t) ;; fontify code in code blocks

(require 'org-latex)
(setq org-export-latex-listings 'minted)
;; Above will output tex files with \usepackage{minted}
;; While in Org mode, `C-c C-e` followed by 'l' and 'l' (twice) will generate the tex

;; In order to have that tex convert to pdf, you have to ensure that you have
;; minted.sty in your TEXMF folder.
;; -> To know if minted.sty in correct path do "kpsewhich minted.sty".
;; -> If it is not found, download from http://www.ctan.org/tex-archive/macros/latex/contrib/minted
;; -> Generate minted.sty by "tex minted.ins"
;; -> To know your TEXMF folder, do "kpsewhich -var-value=TEXMFHOME"
;; -> For me TEXMF folder was ~/texmf
;; -> Move the minted.sty to your $TEXMF/tex/latex/commonstuff folder.
;; -> Do mkdir -p ~/texmf/tex/latex/commonstuff if that folder hierarchy doesn't exist
;; -> Do "mktexlsr" to refresh the sty database
;; -> Generate pdf from the org exported tex by "pdflatex -shell-escape FILE.tex"
;; Sources for org > tex > pdf conversion:
;; -> http://nakkaya.com/2010/09/07/writing-papers-using-org-mode/
;; -> http://mirrors.ctan.org/macros/latex/contrib/minted/minted.pdf

(setq org-log-done 'timestamp) ;; Insert only timestamp when closing an org TODO item
;; (setq org-log-done 'note) ;; Insert timestamp and note when closing an org TODO item
;; Source:http://orgmode.org/manual/Closing-items.html
;; (setq org-startup-indented t) ;; indented headers
(setq org-hide-leading-stars t) ;; hidden leading stars
(setq org-agenda-files (concat org-directory "/agenda.files"))
(setq org-blank-before-new-entry (quote ((heading) (plain-list-item)))) ;; prevent auto blank lines
(setq org-completion-use-ido t) ;; use ido for auto completion
(setq org-return-follows-link t) ;; Hitting <RET> while on a link follows the link
(setq org-startup-folded (quote showeverything))
;; TODO
(setq org-todo-keywords (quote ((sequence "TODO" "SOMEDAY" "DONE"))))
(setq org-enforce-todo-dependencies t) ;; block entries from changing state to DONE
                          ;; while they have children that are not DONE
                          ;; Source: http://orgmode.org/manual/TODO-dependencies.html
;; Capture
(setq org-capture-templates
      '(
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
         "\n* %?\n  Entered on %U")
        ("n" "Note" entry (file "~/org/notes.org")
         "\n* %?\n  Context:\n    %i\n  Entered on %U")
        ("u" "UVM/System Verilog Notes" entry (file "~/org/uvm.org")
         "\n* %?\n  Context:\n    %i\n  Entered on %U")
        ))


(setq setup-org-loaded t)
(provide 'setup-org)
