;; Time-stamp: <2015-07-10 17:52:50 kmodi>

;; Org Mode

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config
  (progn
    (setq org-agenda-archives-mode nil) ; required in org 8.0+
    (setq org-agenda-skip-comment-trees nil)
    (setq org-agenda-skip-function nil)
    (setq org-src-fontify-natively t) ; fontify code in code blocks
    ;; Display entities like \tilde, \alpha, etc in UTF-8 characters
    (setq org-pretty-entities t)
    ;; Allow _ and ^ characters to sub/super-script strings but only when
    ;; string is wrapped in braces
    (setq org-use-sub-superscripts         '{}) ; in-buffer rendering
    (setq org-export-with-sub-superscripts '{}) ; for exports
    ;; Render subscripts and superscripts in org buffers
    (setq org-pretty-entities-include-sub-superscripts t)
    (setq org-export-with-smart-quotes t)
    ;; active single key command execution when at beginning of a headline
    (setq org-use-speed-commands t)
    (setq org-log-done 'timestamp) ; Insert only timestamp when closing an org TODO item
    ;; (setq org-log-done 'note) ; Insert timestamp and note when closing an org TODO item
    ;; http://orgmode.org/manual/Closing-items.html
    (setq org-hide-leading-stars  t)
    ;; Prevent auto insertion of blank lines before headings and list items
    (setq org-blank-before-new-entry '((heading)
                                       (plain-list-item)))
    (setq org-completion-use-ido t) ; use ido for auto completion
    (setq org-return-follows-link t) ; Hitting <RET> while on a link follows the link
    (setq org-startup-folded 'showall)
    ;; fold / overview  - collapse everything, show only level 1 headlines
    ;; content          - show only headlines
    ;; nofold / showall - expand all headlines except the ones with :archive:
    ;;                    tag and property drawers
    ;; showeverything   - same as above but without exceptions
    (setq org-todo-keywords '((sequence "TODO" "SOMEDAY" "CANCELED" "DONE")))
    (setq org-todo-keyword-faces
          '(("TODO"     . org-todo)
            ("SOMEDAY"  . (:foreground "black" :background "#FFEF9F"))
            ("CANCELED" . (:foreground "#94BFF3" :weight bold :strike-through t))
            ("DONE"     . (:foreground "black" :background "#91ba31"))
            ))
    ;; Block entries from changing state to DONE while they have children
    ;; that are not DONE - http://orgmode.org/manual/TODO-dependencies.html
    (setq org-enforce-todo-dependencies t)
    (setq org-catch-invisible-edits 'smart) ; http://emacs.stackexchange.com/a/2091/115
    (setq org-startup-indented t) ; http://orgmode.org/manual/Clean-view.html
    (setq org-indent-indentation-per-level 1) ; default = 2
    (setq org-export-headline-levels 4)

    ;; Number of empty lines needed to keep an empty line between collapsed trees.
    ;; If you leave an empty line between the end of a subtree and the following
    ;; headline, this empty line is hidden when the subtree is folded.
    ;; Org-mode will leave (exactly) one empty line visible if the number of
    ;; empty lines is equal or larger to the number given in this variable.
    (setq org-cycle-separator-lines 2) ; default = 2

    ;; CAPTURE
    ;; http://orgmode.org/manual/Template-elements.html
    ;; http://orgmode.org/manual/Template-expansion.html
    (setq org-capture-templates
          '(("j" "Journal" entry ; `org-capture' binding + j
             (file+datetree (expand-file-name "journal.org" org-directory))
             "\n* %?\n  Entered on %U")
            ("n" "Note" entry ; `org-capture' binding + n
             (file (expand-file-name "notes.org" org-directory))
             "\n* %?\n  Context:\n    %i\n  Entered on %U")
            ("u" "UVM/System Verilog Notes" ; `org-capture' binding + u
             entry (file (expand-file-name "uvm.org" org-directory))
             "\n* %?\n  Context:\n    %i\n  Entered on %U")))

    (defvar modi/one-org-agenda-file (expand-file-name "agenda.files"
                                                       org-directory)
      "One file to contain a list of all org agenda files.")
    (setq org-agenda-files modi/one-org-agenda-file)
    (unless (file-exists-p modi/one-org-agenda-file)
      ;; http://stackoverflow.com/a/14072295/1219634
      ;; touch `modi/one-org-agenda-file'
      (write-region "" :ignore modi/one-org-agenda-file))

    ;; Change the default app for opening pdf files from org
    ;; http://stackoverflow.com/a/9116029/1219634
    (add-to-list 'org-src-lang-modes '("systemverilog" . verilog))
    (add-to-list 'org-src-lang-modes '("dot"           . graphviz-dot))
    ;; Change .pdf association directly within the alist
    (setcdr (assoc "\\.pdf\\'" org-file-apps) "acroread %s")

    ;; (require 'org-latex) in org version < 8.0
    ;; (setq org-export-latex-listings 'minted) in org version < 8.0
    ;; (add-to-list 'org-export-latex-packages-alist '("" "minted")) in org version < 8.0

    (defun my-org-mode-customizations ()
      ;; Reset the `local-set-key' calls
      (local-unset-key (kbd "<f10>"))
      (local-unset-key (kbd "<s-f10>"))
      (local-unset-key (kbd "<S-f10>"))
      (local-unset-key (kbd "<C-f10>")))
    (add-hook 'org-mode-hook #'my-org-mode-customizations)

    ;; http://emacs.stackexchange.com/a/13854/115
    ;; Heading▮   --(C-c C-t)--> * TODO Heading▮
    ;; * Heading▮ --(C-c C-t)--> * TODO Heading▮
    (defun modi/org-first-convert-to-heading (orig-fun &rest args)
      (let ((is-heading))
        (save-excursion
          (forward-line 0)
          (when (looking-at "^\\*")
            (setq is-heading t)))
        (unless is-heading
          (org-toggle-heading))
        (apply orig-fun args)))
    (advice-add 'org-todo :around #'modi/org-first-convert-to-heading)

    ;; Diagrams
    ;; http://pages.sachachua.com/.emacs.d/Sacha.html
    (setq org-ditaa-jar-path (expand-file-name
                              "ditaa.jar"
                              (concat user-emacs-directory "software/")))
    (setq org-plantuml-jar-path (expand-file-name
                                 "plantuml.jar"
                                 (concat user-emacs-directory "software/")))

    ;; (setq org-startup-with-inline-images t)
    ;; (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)

    (org-babel-do-load-languages
     'org-babel-load-languages '((ditaa    . t)   ; activate ditaa
                                 (plantuml . t)   ; activate plantuml
                                 (latex    . t)   ; activate latex
                                 (dot      . t))) ; activate graphviz

    (defun my-org-confirm-babel-evaluate (lang body)
      (and (not (string= lang "ditaa"))    ; don't ask for ditaa
           (not (string= lang "plantuml")) ; don't ask for plantuml
           (not (string= lang "latex"))    ; don't ask for latex
           (not (string= lang "dot"))      ; don't ask for graphviz
           ))
    (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
    (setq org-confirm-elisp-link-function 'yes-or-no-p)

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
                (bind-keys
                 :map org-agenda-mode-map
                  ("x" . sacha/org-agenda-done)
                  ("X" . sacha/org-agenda-mark-done-and-add-followup)
                  ("N" . sacha/org-agenda-new))))

    (bind-keys
     :map modi-mode-map
      ("C-c a" . org-agenda)
      ("C-c c" . org-capture))

    ;; Make `org-return' repeat the number passed through the argument
    (defun modi/org-return-no-indent (&optional n)
      (interactive "p")
      (dotimes (cnt n)
        (org-return nil)))
    (bind-keys
     :map org-mode-map
      ("C-m" . modi/org-return-no-indent))

    ;; http://emacs.stackexchange.com/a/10712/115
    (defun modi/org-delete-link ()
      "Replace an org link of the format [[LINK][DESCRIPTION]] with DESCRIPTION.
If the link is of the format [[LINK]], delete the whole org link.

In both the cases, save the LINK to the kill-ring.

Execute this command while the point is on or after the hyper-linked org link."
      (interactive)
      (when (derived-mode-p 'org-mode)
        (let ((search-invisible t) start end)
          (save-excursion
            (when (re-search-backward "\\[\\[" nil :noerror)
              (when (re-search-forward "\\[\\[\\(.*?\\)\\(\\]\\[.*?\\)*\\]\\]"
                                       nil :noerror)
                (setq start (match-beginning 0))
                (setq end   (match-end 0))
                (kill-new (match-string-no-properties 1)) ; Save link to kill-ring
                (replace-regexp "\\[\\[.*?\\(\\]\\[\\(.*?\\)\\)*\\]\\]" "\\2"
                                nil start end)))))))

    ;; epresent
    (use-package epresent
      :commands (epresent-run))

    ;; org-tree-slide
    ;; https://github.com/takaxp/org-tree-slide
    (use-package org-tree-slide
      :load-path "elisp/org-tree-slide"
      :commands (org-tree-slide-mode)
      :init
      (progn
        (bind-key "<C-S-f8>" #'org-tree-slide-mode modi-mode-map))
      :config
      (progn
        (setq org-tree-slide--lighter " Slide")

        (defvar org-tree-slide-text-scale 3
          "Text scale ratio to default when `org-tree-slide-mode' is enabled.")

        (defun org-tree-slide-my-profile ()
          "Customize org-tree-slide variables.
  `org-tree-slide-header'            => nil
  `org-tree-slide-slide-in-effect'   => nil
  `org-tree-slide-heading-emphasis'  => nil
  `org-tree-slide-cursor-init'       => t
  `org-tree-slide-modeline-display'  => 'lighter
  `org-tree-slide-skip-done'         => nil
  `org-tree-slide-skip-comments'     => t
"
          (interactive)
          (setq org-tree-slide-text-scale         2)
          (setq org-tree-slide-header             nil)
          (setq org-tree-slide-slide-in-effect    nil)
          (setq org-tree-slide-heading-emphasis   nil)
          (setq org-tree-slide-cursor-init        t)
          (setq org-tree-slide-modeline-display   'lighter)
          (setq org-tree-slide-skip-done          nil)
          (setq org-tree-slide-skip-comments      t)
          (setq org-tree-slide-activate-message   "Starting org presentation.")
          (setq org-tree-slide-deactivate-message "Ended presentation."))

        (defun my/org-tree-slide-start ()
          (interactive)
          (modi/toggle-one-window 4) ; force 1 window
          (text-scale-set org-tree-slide-text-scale)
          (org-tree-slide-my-profile))

        (defun my/org-tree-slide-stop()
          (interactive)
          (modi/toggle-one-window) ; toggle 1 window
          (text-scale-set 0))

        (add-hook 'org-tree-slide-play-hook #'my/org-tree-slide-start)
        ;; (remove-hook 'org-tree-slide-play-hook #'my/org-tree-slide-start)
        (add-hook 'org-tree-slide-stop-hook #'my/org-tree-slide-stop)
        ;; (remove-hook 'org-tree-slide-stop-hook #'my/org-tree-slide-stop)

        (bind-key "<left>"   #'org-tree-slide-move-previous-tree                                  org-tree-slide-mode-map)
        (bind-key "<right>"  #'org-tree-slide-move-next-tree                                      org-tree-slide-mode-map)
        (bind-key "C-0"      (lambda () (interactive) (text-scale-set org-tree-slide-text-scale)) org-tree-slide-mode-map)
        (bind-key "C-="      (lambda () (interactive) (text-scale-increase 1))                    org-tree-slide-mode-map)
        (bind-key "C--"      (lambda () (interactive) (text-scale-decrease 1))                    org-tree-slide-mode-map)
        (bind-key "C-1"      #'org-tree-slide-content                                             org-tree-slide-mode-map)
        (bind-key "C-2"      #'org-tree-slide-my-profile                                          org-tree-slide-mode-map)
        (bind-key "C-3"      #'org-tree-slide-simple-profile                                      org-tree-slide-mode-map)
        (bind-key "C-4"      #'org-tree-slide-presentation-profile                                org-tree-slide-mode-map)))

    (use-package ox
      :commands (org-export-dispatch) ; bound to `C-c C-e' in org-mode
      :config
      (progn
        ;; LaTeX export
        (use-package ox-latex
          :config
          (progn
            ;; ox-latex patches
            (load (expand-file-name
                   "ox-latex-patches.el"
                   (concat user-emacs-directory "elisp/patches/"))
                  nil :nomessage)
            ;; Previewing latex fragments in org mode
            ;; http://orgmode.org/worg/org-tutorials/org-latex-preview.html
            ;; (setq org-latex-create-formula-image-program 'dvipng) ; NOT Recommended
            (setq org-latex-create-formula-image-program 'imagemagick) ; Recommended
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
                    ;; http://en.wikibooks.org/wiki/LaTeX/Paragraph_Formatting
                    (""          "parskip")
                    ;; ;; Replace default font with a much crisper font
                    ;; ;; http://www.khirevich.com/latex/font/
                    ;; (""          "charter")
                    ;; ("expert"    "mathdesign")
                    ;; Code blocks syntax highlighting
                    ;; (""          "listings")
                    ;; (""          "xcolor")
                    (""          "minted") ; Comment this if org-latex-create-formula-image-program
                    ;; is set to dvipng. minted package can't be loaded
                    ;; when using dvipng to show latex previews
                    ;; (""          "minted" nil) ; Uncomment this if org-latex-create-formula-image-program
                    ;;                            ; is set to dvipng
                    ;; Graphics package for more complicated figures
                    (""          "tikz")
                    ;; Prevent tables/figures from one section to float into another section
                    ;; http://tex.stackexchange.com/questions/279/how-do-i-ensure-that-figures-appear-in-the-section-theyre-associated-with
                    ("section"   "placeins")
                    ;; It doesn't seem below packages are required
                    ;; ;; Packages suggested to be added for previewing latex fragments
                    ;; ;; http://orgmode.org/worg/org-tutorials/org-latex-preview.html
                    ;; ("usenames"  "color") ; HAD TO COMMENT IT OUT BECAUSE OF CLASH WITH placeins pkg
                    ("mathscr"   "eucal")
                    (""          "latexsym")
                    ;; Prevent an image from floating to a different location
                    ;; http://tex.stackexchange.com/questions/8625/force-figure-placement-in-text
                    (""          "float")
                    (""          "caption")
                    ))

            ;; "H" option is from the `float' package. That prevents the images from floating around.
            ;; (setq org-latex-default-figure-position "htb") ; default - figures are floating
            (setq org-latex-default-figure-position "H") ; figures are NOT floating

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

            ;; http://orgmode.org/worg/org-faq.html#using-xelatex-for-pdf-export
            ;; latexmk runs pdflatex/xelatex (whatever is specified) multiple times
            ;; automatically to resolve the cross-references.
            ;; (setq org-latex-pdf-process '("latexmk -xelatex -quiet -shell-escape -f %f"))

            ;; Run xelatex multiple times to get the cross-references right
            (setq org-latex-pdf-process '("xelatex -shell-escape %f"
                                          "xelatex -shell-escape %f"
                                          "xelatex -shell-escape %f"
                                          "xelatex -shell-escape %f"))

            ;; Run pdflatex multiple times to get the cross-references right
            ;; (setq org-latex-pdf-process '("pdflatex -shell-escape %f"
            ;; "pdflatex -shell-escape %f"
            ;; "pdflatex -shell-escape %f"))

            ;; minted package options (applied to embedded source codes)
            ;; https://github.com/gpoore/minted
            (setq org-latex-minted-options
                  '(("linenos")
                    ("numbersep"   "5pt")
                    ("frame"       "none") ; box frame is created by the mdframed package
                    ("framesep"    "2mm")
                    ;; ("fontfamily"  "zi4") ; required only when using pdflatex
                                        ; instead of xelatex
                    ;; minted 2.0 specific features
                    ("breaklines") ; line wrapping within code blocks
                    ))
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
            ))

        ;; HTML export
        (use-package ox-html
          :config
          (progn
            ;; ox-html patches
            (load (expand-file-name
                   "ox-html-patches.el"
                   (concat user-emacs-directory "elisp/patches/"))
                  nil :nomessage)

            (use-package ox-html-fancybox
              :load-path "elisp/ox-html-fancybox")

            ;; Center align the tables when exporting to HTML
            ;; Note: This aligns the whole table, not the table columns
            (setq org-html-table-default-attributes
                  '(:border "2"
                    :cellspacing "0"
                    :cellpadding "6"
                    :rules "groups"
                    :frame "hsides"
                    :align "center"
                    ;; below class requires bootstrap.css ( http://getbootstrap.com )
                    :class "table-striped")
                  ;; '(:border "2"
                  ;;           :cellspacing "0"
                  ;;           :cellpadding "6"
                  ;;           :rules "groups"
                  ;;           :frame "hsides")
                  )

            ;; Customize the HTML postamble
            (setq org-html-postamble t) ; default value = 'auto
            (setq org-html-postamble-format
                  '(("en" "Exported using <div style=\"display: inline\" class=\"creator\">%c</div> on <div style=\"display: inline\"class=\"date\">%d</div> by %e.")))

            ;; (setq org-html-htmlize-output-type 'inline-css) ; default
            (setq org-html-htmlize-output-type 'css)
            ;; (setq org-html-htmlize-font-prefix "") ; default
            (setq org-html-htmlize-font-prefix "org-")

            (defvar modi/htmlize-html-file (concat user-home-directory
                                                   "temp/htmlize_temp.html")
              "Name of the html file exported by `modi/htmlize-region-as-html-file'.")
            (defvar modi/htmlize-css-file (concat user-emacs-directory
                                                  "misc/css/leuven_theme.css")
              "CSS file to be embedded in the html file created using the
             `modi/htmlize-region-as-html-file' function.")
            (defun modi/htmlize-region-as-html-file (open-in-browser)
              "Export the selected region to an html file. If a region is not
             selected, export the whole buffer.

             If OPEN-IN-BROWSER is non-nil, also open the exported html file in the
             default browser."
              (interactive "P")
              (let ((org-html-htmlize-output-type 'css)
                    (org-html-htmlize-font-prefix "org-")
                    start end html-string)
                (if (use-region-p)
                    (progn
                      (setq start (region-beginning))
                      (setq end   (region-end)))
                  (progn
                    (setq start (point-min))
                    (setq end   (point-max))))
                (setq html-string (org-html-htmlize-region-for-paste start end))
                (with-temp-buffer
                  (insert-file-contents modi/htmlize-css-file nil nil nil :replace)
                  (goto-char (point-min))
                  (insert (concat "<!-- This file is generated using the "
                                  "modi/htmlize-region-as-html-file function\n"
                                  "from https://github.com/kaushalmodi/.emacs.d/"
                                  "blob/master/setup-files/setup-org.el -->\n"))
                  (insert "<html>\n<head>\n<style media=\"screen\" type=\"text/css\">\n")
                  (goto-char (point-max))
                  (insert "</style>\n</head>\n<body>\n")
                  (insert html-string)
                  (insert "</body>\n</html>\n")
                  (write-file modi/htmlize-html-file)
                  (when open-in-browser
                    (browse-url-of-file modi/htmlize-html-file)))))
            (bind-key "H" #'modi/htmlize-region-as-html-file region-bindings-mode-map)))

        ;; Beamer export
        (use-package ox-beamer
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

        ;; Presentations using reveal.js
        ;; Download reveal.js from https://github.com/hakimel/reveal.js/
        (use-package ox-reveal
          :config
          (progn
            (setq org-reveal-root    (concat user-emacs-directory "software/reveal.js/"))
            (setq org-reveal-hlevel  1)
            (setq org-reveal-theme   "default") ; beige blood moon night serif simple sky solarized
            (setq org-reveal-mathjax t))) ; Use mathjax.org to render LaTeX equations

        ;; ODT, doc export
        ;; http://stackoverflow.com/a/22990257/1219634
        (use-package ox-odt
          ;; :disabled
          :config
          (progn
            ;; Auto convert the exported .odt to .doc (MS Word 97) format
            ;; Requires the soffice binary packaged with openoffice
            (setq org-odt-preferred-output-format "doc")))

        (defun modi/org-export-to-html-txt-pdf ()
          "Export the org file to multiple formats."
          (interactive)
          (org-html-export-to-html)
          (org-ascii-export-to-ascii)
          (org-latex-export-to-pdf))

        ;; Auto update line numbers for source code includes when exporting
        (use-package org-include-src-lines
          :load-path "elisp/org-include-src-lines")

        ;; Replace include pdf files with images when exporting
        (use-package org-include-img-from-pdf
          :load-path "elisp/org-include-img-from-pdf")

        ;; Auto extract images from zip files
        (use-package org-include-img-from-archive
          :load-path "elisp/org-include-img-from-archive")
        ))

    ;; Support markdown-style link ids
    (use-package org-linkid
      :load-path "elisp/org-linkid")

    ;; Update TODAY macro with current date
    (add-hook 'org-export-before-processing-hook #'modi/org-update-TODAY-macro)
    (defun modi/org-update-TODAY-macro (&rest ignore)
      "Update TODAY macro to hold string with current date."
      (interactive)
      (when (derived-mode-p 'org-mode)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward
                  "^\\s-*#\\+MACRO:\\s-+TODAY"
                  nil 'noerror)
            (forward-line 0)
            (when (looking-at ".*TODAY\\(.*\\)")
              (replace-match
               (concat " "
                       (format-time-string "%b %d %Y, %a" (current-time)))
               :fixedcase :literal nil 1))))))

    ;; http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
    ;; Pressing `C-x C-s' while editing org source code blocks saves and exits
    ;; the edit.
    (with-eval-after-load 'org-src
      (bind-key "C-x C-s" #'org-edit-src-exit org-src-mode-map))

    ;; http://orgmode.org/manual/Easy-Templates.html
    ;; http://oremacs.com/2015/03/07/hydra-org-templates
    ;; https://github.com/abo-abo/hydra/wiki/Org-mode-block-templates
    (defhydra hydra-org-template (:color blue :hint nil)
      "
_c_enter  _q_uote     _e_macs-lisp    _L_aTeX:
_l_atex   e_x_ample   _v_erilog       _i_ndex:
_a_scii   _v_erse     _m_atlab        _I_NCLUDE:
_s_rc     ^^          _S_hell         _H_TML:
_h_tml    ^^          _t_ext          _A_SCII:
"
      ("s" (hot-expand "<s")) ; #+BEGIN_SRC ... #+END_SRC
      ("e" (progn
             (hot-expand "<s")
             (insert "emacs-lisp")
             (forward-line)))
      ("v" (progn
             (hot-expand "<s")
             (insert "systemverilog")
             (forward-line)))
      ("m" (progn
             (hot-expand "<s")
             (insert "matlab")
             (forward-line)))
      ("S" (progn
             (hot-expand "<s")
             (insert "sh")
             (forward-line)))
      ("t" (progn
             (hot-expand "<s")
             (insert "text")
             (forward-line)))
      ("x" (hot-expand "<e")) ; #+BEGIN_EXAMPLE ... #+END_EXAMPLE
      ("q" (hot-expand "<q")) ; #+BEGIN_QUOTE ... #+END_QUOTE
      ("V" (hot-expand "<v")) ; #+BEGIN_VERSE ... #+END_VERSE
      ("c" (hot-expand "<c")) ; #+BEGIN_CENTER ... #+END_CENTER
      ("l" (hot-expand "<l")) ; #+BEGIN_LaTeX ... #+END_LaTeX
      ("L" (hot-expand "<L")) ; #+LaTeX:
      ("h" (hot-expand "<h")) ; #+BEGIN_HTML ... #+END_HTML
      ("H" (hot-expand "<H")) ; #+HTML:
      ("a" (hot-expand "<a")) ; #+BEGIN_ASCII ... #+END_ASCII
      ("A" (hot-expand "<A")) ; #+ASCII:
      ("i" (hot-expand "<i")) ; #+INDEX: line
      ("I" (hot-expand "<I")) ; #+INCLUDE: line
      ("<" self-insert-command "<")
      ("o" nil         "quit"))

    (defun hot-expand (str)
      "Expand org template."
      (insert str)
      (org-try-structure-completion))

    (define-key org-mode-map "<" (lambda ()
                                   (interactive)
                                   (if (looking-back "^")
                                       (hydra-org-template/body)
                                     (self-insert-command 1))))
    ))


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
;; ​* M-TAB <- Headlines; useful when doing [[* Partial heading M-TAB when linking to headings
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

;; Disable selected org-mode markup character on per-file basis
;; http://emacs.stackexchange.com/a/13231/115

;; How to modify `org-emphasis-regexp-components'
;; http://emacs.stackexchange.com/a/13828/115
