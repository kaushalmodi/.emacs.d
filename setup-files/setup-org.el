;; Time-stamp: <2014-08-13 12:29:52 kmodi>

;; Org Mode

(setq org-agenda-archives-mode nil ;; required in org 8.0+
      org-agenda-skip-comment-trees nil
      org-agenda-skip-function nil
      org-src-fontify-natively t ;; fontify code in code blocks
      org-pretty-entities t ;; Display entities like \tilde, \alpha, etc in UTF-8 characters
      org-pretty-entities-include-sub-superscripts nil ;; Display entities like \tilde, \alpha, etc in UTF-8 characters
      org-export-with-smart-quotes t
      ;; active single key command execution when at beginning of a headline
      org-use-speed-commands t
      ;; Allow _ and ^ characters to sub/super-script strings but only when followed by braces
      org-use-sub-superscripts         '{}
      org-export-with-sub-superscripts '{}
      ;; Insert only timestamp when closing an org TODO item
      org-log-done 'timestamp
      ;; org-log-done 'note ;; Insert timestamp and note when closing an org TODO item
      ;; Source:http://orgmode.org/manual/Closing-items.html
      org-hide-leading-stars  t ;; hidden leading stars
      org-agenda-files (concat org-directory "/agenda.files")
      org-blank-before-new-entry (quote ((heading) (plain-list-item))) ;; prevent auto blank lines
      org-completion-use-ido t ;; use ido for auto completion
      org-return-follows-link t ;; Hitting <RET> while on a link follows the link
      org-startup-folded (quote showeverything)
      org-todo-keywords (quote ((sequence "TODO" "SOMEDAY" "CANCELED" "DONE")))
      org-todo-keyword-faces
      '(("TODO"     . org-warning)
        ("SOMEDAY"  . "#FFEF9F")
        ("CANCELED" . (:foreground "#94BFF3" :weight bold :strike-through t)))
      ;; block entries from changing state to DONE while they have children
      ;; that are not DONE
      ;; Source: http://orgmode.org/manual/TODO-dependencies.html
      org-enforce-todo-dependencies t
      ;; Capture
      ;; By default, org capture dialog is activated by `C-c c`
      org-capture-templates
      '(
        ("j" "Journal" entry ;; C-c c j
         (file+datetree (concat org-directory "/journal.org"))
         "\n* %?\n  Entered on %U")
        ("n" "Note" entry ;; C-c c n
         (file (concat org-directory "/notes.org"))
         "\n* %?\n  Context:\n    %i\n  Entered on %U")
        ("u" "UVM/System Verilog Notes" ;; C-c c u
         entry (file (concat org-directory "/uvm.org"))
         "\n* %?\n  Context:\n    %i\n  Entered on %U")))

(when (boundp 'project1-org-dir) ;; set in setup-secret.el
  (add-to-list 'org-capture-templates
               '("t" "Project 1 Meeting Notes" entry
                 (file+datetree
                  (concat project1-org-dir "/dv_meeting_notes.org")) ;; C-c c t
                 "\n* %?\n  Entered on %U")))

;; change the default app for opening pdf files from org
;; Source: http://stackoverflow.com/questions/8834633/how-do-i-make-org-mode-open-pdf-files-in-evince
(eval-after-load "org"
  '(progn
     (add-to-list 'org-src-lang-modes '("systemverilog" . verilog))
     (add-to-list 'org-src-lang-modes '("dot"           . graphviz-dot))
     ;; Change .pdf association directly within the alist
     (setcdr (assoc "\\.pdf\\'" org-file-apps) "acroread %s")))

;; (require 'org-latex) in org version < 8.0
;; (setq org-export-latex-listings 'minted) in org version < 8.0
;; (add-to-list 'org-export-latex-packages-alist '("" "minted")) in org version < 8.0

;; From <ORG EL DIR>/ox-latex.el
(req-package ox-latex
  :config
  (progn
    ;; Previewing latex fragments in org mode
    ;; Source: http://orgmode.org/worg/org-tutorials/org-latex-preview.html
    ;; (setq org-latex-create-formula-image-program 'dvipng) ;; NOT Recommended
    (setq org-latex-create-formula-image-program 'imagemagick) ;; Recommended
    (setq org-latex-listings 'minted)
    ;; (setq org-latex-listings 'lstlisting)

    ;; Below will output tex files with \usepackage[FIRST STRING IF NON-EMPTY]{SECOND STRING}
    ;; The org-latex-packages-alist is a list of cells of the format:
    ;; ("options" "package" snippet-flag)
    ;; snippet-flag is non-nil by default. So unless this flag is set to nil, that
    ;; package will be used even when previewing latex fragments using the `C-c C-x C-l`
    ;; org mode key binding
    (setq org-latex-packages-alist
          '(
            ;; % 0 paragraph indent, adds vertical space between paragraphs
            ;; Source: http://en.wikibooks.org/wiki/LaTeX/Paragraph_Formatting
            (""          "parskip")
            ;; ;; Replace default font with a much crisper font
            ;; ;; Source: http://www.khirevich.com/latex/font/
            ;; (""          "charter")
            ;; ("expert"    "mathdesign")
            ;; Code blocks syntax highlighting
            ;; (""          "listings")
            ;; (""          "xcolor")
            (""          "minted") ;; Comment this if org-latex-create-formula-image-program
            ;; is set to dvipng. minted package can't be loaded
            ;; when using dvipng to show latex previews
            ;; (""          "minted" nil) ;; Uncomment this if org-latex-create-formula-image-program
            ;;                            ;; is set to dvipng
            ;; Graphics package for more complicated figures
            (""          "tikz")
            ;; Prevent tables/figures from one section to float into another section
            ;; Source: http://tex.stackexchange.com/questions/279/how-do-i-ensure-that-figures-appear-in-the-section-theyre-associated-with
            ("section"   "placeins")
            ;; It doesn't seem below packages are required
            ;; ;; Packages suggested to be added for previewing latex fragments
            ;; ;; Source: http://orgmode.org/worg/org-tutorials/org-latex-preview.html
            ;; ("usenames"  "color") ;; HAD TO COMMENT IT OUT BECAUSE OF CLASH WITH placeins pkg
            ("mathscr"   "eucal")
            (""          "latexsym")
            ;; Prevent an image from floating to a different location
            ;; http://tex.stackexchange.com/questions/8625/force-figure-placement-in-text
            (""          "float")
            (""          "caption")
            ))

    ;; "H" option is from the `float' package. That prevents the images from floating around.
    ;; (setq org-latex-default-figure-position "htb") ;; default - figures are floating
    (setq org-latex-default-figure-position "H") ;; figures are NOT floating

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

    ;; -shell-escape is required when converting a .tex file containing `minted'
    ;; package to a .pdf
    ;; Now org > tex > pdf conversion can happen with the org default
    ;; `C-c C-e l p` key binding

    ;; Source: http://orgmode.org/worg/org-faq.html#using-xelatex-for-pdf-export
    ;; latexmk runs pdflatex/xelatex (whatever is specified) multiple times
    ;; automatically to resolve the cross-references.
    ;; (setq org-latex-pdf-process '("latexmk -xelatex -quiet -shell-escape %f"))

    ;; Run xelatex multiple times to get the cross-references right
    (setq org-latex-pdf-process '("xelatex -shell-escape %f"
                                  "xelatex -shell-escape %f"
                                  "xelatex -shell-escape %f"))

    ;; Run pdflatex multiple times to get the cross-references right
    ;; (setq org-latex-pdf-process '("pdflatex -shell-escape %f"
    ;; "pdflatex -shell-escape %f"
    ;; "pdflatex -shell-escape %f"))

    ;; customization of the minted package (applied to embedded source codes)
    ;; Source: https://code.google.com/p/minted/
    (setq org-latex-minted-options
          '(("linenos")
            ("numbersep"   "5pt")
            ("frame"       "none") ;; box frame is created by the mdframed package
            ("framesep"    "2mm")
            ;; ("fontfamily"  "zi4") ;; Required only when using pdflatex instead of xelatex
            ))))

    ;; (add-to-list 'org-latex-classes
    ;;              '("article"
    ;;                "\\documentclass[11pt,letterpaper]{article}"
    ;;                ("\\section{%s}"        . "\\section*{%s}")
    ;;                ("\\subsection{%s}"     . "\\subsection*{%s}")
    ;;                ("\\subsubsection{%s}"  . "\\subsubsection*{%s}")
    ;;                ("\\paragraph{%s}"      . "\\paragraph*{%s}")
    ;;                ("\\subparagraph{%s}"   . "\\subparagraph*{%s}"))
    ;;              )

;; You can also do the org > tex > pdf conversion and open the pdf file in
;; acroread directly using the `C-c C-e l o` key binding

(req-package ox-beamer
  :commands (org-beamer-export-as-latex
             org-beamer-export-to-latex
             org-beamer-export-to-pdf)
  :config
  (progn
    ;; allow for export=>beamer by placing
    ;; #+LaTeX_CLASS: beamer in org files
    (add-to-list 'org-latex-classes
                 '("beamer"
                   "\\documentclass[presentation]{beamer}"
                   ("\\section{%s}"        . "\\section*{%s}")
                   ("\\subsection{%s}"     . "\\subsection*{%s}")
                   ("\\subsubsection{%s}"  . "\\subsubsection*{%s}")))))

;; Below define-key sets to nil not required now that the key bindings in
;; modi-mode minor mode get the highest priority
;; (defun my-org-mode-customizations ()
;;   ;; Remove the org mode binding that conflicts with ace-jump-mode binding
;;   (define-key org-mode-map (kbd "C-c SPC") nil)
;;   ;; Override the org-mode binding for `C-j'; use my custom global key binding
;;   (define-key org-mode-map (kbd "C-j") nil)
;;   )
;; (add-hook 'org-mode-hook 'my-org-mode-customizations)

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
   (dot      . t))) ;; activate graphviz

(defun my-org-confirm-babel-evaluate (lang body)
  (and (not (string= lang "ditaa"))    ;; don't ask for ditaa
       (not (string= lang "plantuml")) ;; don't ask for plantuml
       (not (string= lang "latex"))    ;; don't ask for latex
       (not (string= lang "dot"))      ;; don't ask for graphviz
       ))
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
(setq org-confirm-elisp-link-function 'yes-or-no-p)

;; Presentations using reveal.js
;; Download reveal.js from https://github.com/hakimel/reveal.js/
;; Instead `ox-reveal' from MELPA
(req-package ox-reveal
  :config
  (progn
    ;; I have git clones reveal.js in my {emacs config directory}/from-git/
    (setq org-reveal-root     (concat "file://" user-emacs-directory "/from-git/reveal.js/")
          org-reveal-hlevel   1
          org-reveal-theme    "default" ;; beige blood moon night serif simple sky solarized
          org-reveal-mathjax  t))) ;; Use mathjax.org to render LaTeX equations

;; org-agenda related functions
;; http://sachachua.com/blog/2013/01/emacs-org-task-related-keyboard-shortcuts-agenda/
(defun sacha/org-agenda-done (&optional arg)
  "Mark current TODO as done.
This changes the line at point, all other lines in the agenda referring to
the same tree node, and the headline of the tree node in the Org-mode file."
  (interactive "P")
  (org-agenda-todo "DONE"))

(defun sacha/org-agenda-mark-done-and-add-followup ()
  "Mark the current TODO as done and add another task after it.
Creates it at the same level as the previous task, so it's better to use
this with to-do items than with projects or headings."
  (interactive)
  (org-agenda-todo "DONE")
  (org-agenda-switch-to)
  (org-capture 0 "t")
  (org-metadown 1)
  (org-metaright 1))

(defun sacha/org-agenda-new ()
  "Create a new note or task at the current agenda item.
Creates it at the same level as the previous task, so it's better to use
this with to-do items than with projects or headings."
  (interactive)
  (org-agenda-switch-to)
  (org-capture 0))

(add-hook 'org-agenda-mode-hook
          (lambda ()
            ;; Override the key definition for org-exit
            (define-key org-agenda-mode-map "x" 'sacha/org-agenda-done)
            (define-key org-agenda-mode-map "X" 'sacha/org-agenda-mark-done-and-add-followup)
            ;; New key assignment
            (define-key org-agenda-mode-map "N" 'sacha/org-agenda-new)))

;; org-ref - JKitchin
;; http://kitchingroup.cheme.cmu.edu/blog/2014/05/13/Using-org-ref-for-citations-and-references/
;; (org-babel-load-file (concat user-emacs-directory "/from-git/org-ref/org-ref.org"))

;; org-show -JKitchin
;; https://github.com/jkitchin/jmax/blob/master/org-show.org
(req-package org-show
  :commands (org-show))
;; (define-key  org-show-mode-map  [next]        'org-show-next-slide) ;; Pg-Down
;; (define-key  org-show-mode-map  [prior]       'org-show-previous-slide) ;; Pg-Up

;; (define-key  org-show-mode-map  [f5]          'org-show-start-slideshow)
;; (define-key  org-show-mode-map  [f6]          'org-show-execute-slide)
;; (define-key  org-show-mode-map  (kbd "C--")   'org-show-decrease-text-size)
;; (define-key  org-show-mode-map  (kbd "C-=")   'org-show-increase-text-size)
;; (define-key  org-show-mode-map  (kbd "\e\eg") 'org-show-goto-slide)
;; (define-key  org-show-mode-map  (kbd "\e\et") 'org-show-toc)
;; (define-key  org-show-mode-map  (kbd "\e\eq") 'org-show-stop-slideshow)

;; Key bindings
(bind-keys
 :map modi-mode-map
 ("C-c a" . org-agenda)
 ("C-c b" . org-iswitchb)
 ("C-c c" . org-capture))


(provide 'setup-org)

;; NOTES
;; C-c C-e l l <-- Do org > tex
;; C-c C-e l p <-- Do org > tex > pdf using the command specified in
;;                 `org-latex-pdf-process' list
;; C-c C-e l o <-- Do org > tex > pdf and open the pdf file using the app
;;                 associated with pdf files defined in `org-file-apps'

;; When the cursor is on an existing link, `C-c C-l' allows you to edit the link
;; and description parts of the link.

;; C-c C-x f <-- Insert footnote

;; C-c C-x C-l <-- Preview latex fragment in place; Press C-c C-c to exit that preview.

;; Auto-completions http://orgmode.org/manual/Completion.html
;; \ M-TAB <- TeX symbols
;; * M-TAB <- Headlines; useful when doing [[* Partial heading M-TAB when linking to headings
;; #+ M-TAB <- org-mode special keywords like #+DATE, #+AUTHOR, etc

;; Speed-keys are awesome! http://orgmode.org/manual/Speed-keys.html

;; Easy Templates http://orgmode.org/manual/Easy-Templates.html
;; To insert a structural element, type a ‘<’, followed by a template selector
;; and <TAB>. Completion takes effect only when the above keystrokes are typed
;; on a line by itself.
;; s 	#+BEGIN_SRC ... #+END_SRC
;; e 	#+BEGIN_EXAMPLE ... #+END_EXAMPLE
;; l 	#+BEGIN_LaTeX ... #+END_LaTeX
;; L 	#+LaTeX:
;; h 	#+BEGIN_HTML ... #+END_HTML
;; H 	#+HTML:
;; I 	#+INCLUDE: line
