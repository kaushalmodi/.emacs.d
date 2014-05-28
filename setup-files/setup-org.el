;; Time-stamp: <2014-05-28 11:56:52 kmodi>

;; Org Mode

(setq org-directory "~/org")

(setq org-src-fontify-natively t) ;; fontify code in code blocks

;; From <ORG EL DIR>/ox-latex.el
(require 'ox-latex)
;; (require 'org-latex) in org version < 8.0
(setq org-latex-listings 'minted)
;; (setq org-export-latex-listings 'minted) in org version < 8.0
(add-to-list 'org-latex-packages-alist '("" "minted"))
(add-to-list 'org-latex-packages-alist '("" "tikz"))
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

;; -shell-escape is required when converting a .tex file containing minted
;; package to a .pdf
;; Now org > tex > pdf conversion can happen with the org default
;; `C-c C-e l p` key binding
(setq org-latex-pdf-process '("pdflatex -shell-escape %f"))

;; You can also do the org > tex > pdf conversion and open the pdf file in
;; acroread directly using the `C-c C-e l o` key binding
;; change the default app for opening pdf files from org
;; Source: http://stackoverflow.com/questions/8834633/how-do-i-make-org-mode-open-pdf-files-in-evince
(eval-after-load "org"
  '(progn
     ;; Change .pdf association directly within the alist
     (setcdr (assoc "\\.pdf\\'" org-file-apps) "acroread %s")))

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

;; Diagrammmms
;; Source: http://pages.sachachua.com/.emacs.d/Sacha.html
(setq org-ditaa-jar-path (concat user-emacs-directory "/ditaa/ditaa0_9.jar"))
(setq org-plantuml-jar-path (concat user-emacs-directory "/plantuml/plantuml.7999.jar"))

;; (setq org-startup-with-inline-images t)
;; (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (ditaa    . t) ;; activate ditaa
   (plantuml . t) ;; activate plantuml
   (latex    . t) ;; activate latex
   (dot      . t) ;; activate graphviz
   ))

(add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))

(defun my-org-confirm-babel-evaluate (lang body)
  (and (not (string= lang "ditaa"))    ;; don't ask for ditaa
       (not (string= lang "plantuml")) ;; don't ask for plantuml
       (not (string= lang "latex"))    ;; don't ask for latex
       (not (string= lang "dot"))      ;; don't ask for graphviz
       ))
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)


(setq setup-org-loaded t)
(provide 'setup-org)

;; NOTES
;; C-c C-e l l <-- Do org > tex
;; C-c C-e l p <-- Do org > tex > pdf using the command specified in
;;                 `org-latex-pdf-process' list
;; C-c C-e l o <-- Do org > tex > pdf and open the pdf file using the app
;;                 associated with pdf files defined in `org-file-apps'
