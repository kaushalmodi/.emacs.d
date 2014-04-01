;; Time-stamp: <2014-03-26 23:30:28 kmodi>

;; Org Mode

(setq org-directory "~/org")

(setq org-src-fontify-natively t) ;; fontify code in code blocks

;; From <ORG EL DIR>/ox-latex.el
(require 'ox-latex)
;; (require 'org-latex) in org version < 8.0
(setq org-latex-listings 'minted)
;; (setq org-export-latex-listings 'minted) in org version < 8.0
(add-to-list 'org-latex-packages-alist '("" "minted"))
;; (add-to-list 'org-export-latex-packages-alist '("" "minted")) in org version < 8.0
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

;; customization of the minted package (applied to embedded source codes)
;; Source: https://code.google.com/p/minted/
(setq org-latex-minted-options
      '(("linenos")
        ("numbersep" "5pt")
        ("frame" "lines")
        ("framesep" "2mm")
        ))

(add-to-list 'org-latex-classes
             '("article"
               "\\documentclass[11pt,letterpaper]{article}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
             )

;; FIXME: Make the below work
;; (setq org-latex-to-pdf-process
;;            '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;              "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;              "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; Disallow _ and ^ characters from sub/super-scripting strings
(setq org-export-with-sub-superscripts nil)

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

(defun my-org-mode-customizations ()
  ;; Remove the org mode binding that conflicts with ace-jump-mode binding
  (define-key org-mode-map (kbd "C-c SPC") nil)
  )
(add-hook 'org-mode-hook 'my-org-mode-customizations)


(setq setup-org-loaded t)
(provide 'setup-org)
