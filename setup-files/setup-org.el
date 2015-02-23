;; Time-stamp: <2015-02-23 11:42:33 kmodi>

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
(use-package ox-latex
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

(defun my-org-mode-customizations ()
  ;; Reset the `local-set-key' calls
  (local-unset-key (kbd "<f10>"))
  (local-unset-key (kbd "<s-f10>"))
  (local-unset-key (kbd "<S-f10>"))
  (local-unset-key (kbd "<C-f10>")))
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
   (dot      . t))) ;; activate graphviz

(defun my-org-confirm-babel-evaluate (lang body)
  (and (not (string= lang "ditaa"))    ;; don't ask for ditaa
       (not (string= lang "plantuml")) ;; don't ask for plantuml
       (not (string= lang "latex"))    ;; don't ask for latex
       (not (string= lang "dot"))      ;; don't ask for graphviz
       ))
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
(setq org-confirm-elisp-link-function 'yes-or-no-p)

(defun modi/org-export-to-html-txt-pdf ()
  "Export the org file to multiple formats."
  (interactive)
  (org-html-export-to-html)
  (org-ascii-export-to-ascii)
  (org-latex-export-to-pdf))

;; Presentations using reveal.js
;; Download reveal.js from https://github.com/hakimel/reveal.js/
(use-package ox-reveal
  ;; :load-path "from-git/org-reveal"
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
;; (use-package org-show
;; :load-path "from-git/org-show")
;; (define-key  org-show-mode-map  [next]        'org-show-next-slide) ;; Pg-Down
;; (define-key  org-show-mode-map  [prior]       'org-show-previous-slide) ;; Pg-Up

;; (define-key  org-show-mode-map  [f5]          'org-show-start-slideshow)
;; (define-key  org-show-mode-map  [f6]          'org-show-execute-slide)
;; (define-key  org-show-mode-map  (kbd "C--")   'org-show-decrease-text-size)
;; (define-key  org-show-mode-map  (kbd "C-=")   'org-show-increase-text-size)
;; (define-key  org-show-mode-map  (kbd "\e\eg") 'org-show-goto-slide)
;; (define-key  org-show-mode-map  (kbd "\e\et") 'org-show-toc)
;; (define-key  org-show-mode-map  (kbd "\e\eq") 'org-show-stop-slideshow)

;; epresent
(use-package epresent)

;; org-tree-slide
(use-package org-tree-slide
  ;; :load-path "from-git/org-tree-slide"
  :config
  (progn
    (setq org-tree-slide-slide-in-effect nil)
    (bind-keys
     :map org-tree-slide-mode-map
     ("p" . org-tree-slide-move-previous-tree)
     ("n" . org-tree-slide-move-next-tree)
     ("q" . org-tree-slide-mode))
    (bind-keys
     ("<C-S-f8>" . org-tree-slide-mode))
    ))

;; Key bindings
(bind-keys
 :map modi-mode-map
 ("C-c a" . org-agenda)
 ("C-c b" . org-iswitchb)
 ("C-c c" . org-capture))

;; Updating the #+INCLUDE source code line numbers automatically
;; Source: http://emacs.stackexchange.com/questions/64/how-to-auto-calculate-the-begin-and-end-lines-when-including-source-files-in-org

;; Here is another option. This one is let's you customize the regular
;; expressions on a per-include basis. It should fit better with some
;; workflows as you're not limited to extension-based definitions.

;; * To Use *
;; Do something like the following in your org-file. (The :lines keyword
;; is optional)

;; #+INCLUDE: "code/my-class.sv" :src systemverilog :range-begin "^class"
;;  :range-end "^endclass" :lines "14-80"

;; The function will visit "my-class.sv" and search for those two
;; regexps, and then it will update the :lines keyword according with the
;; match result.

;; If :range-begin is missing, the range will be "-80".
;; If :range-end is missing, the range will be "14-".
;; If both of the above are missing, the `:lines' won't be auto updated

;; Execute the `endless/update-includes' function just before saving the file
(add-hook 'before-save-hook #'endless/update-includes)

;; * The Updater *
;; This is the function that goes through the buffer. You can bind it
;; to a key, or add it to a hook. The following code updates the lines
;; whenever you save the file, but if your use case is different, just
;; find out which hook you need! (org-mode is full of hooks)

(defun endless/update-includes (&rest ignore)
  "Update the line numbers of #+INCLUDE:s in current buffer.
Only looks at INCLUDEs that have either :range-begin or :range-end.
This function does nothing if not in org-mode, so you can safely
add it to `before-save-hook'."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp
              "^\\s-*#\\+INCLUDE: *\"\\([^\"]+\\)\".*:range-\\(begin\\|end\\)"
              nil 'noerror)
        (let* ((file (expand-file-name (match-string-no-properties 1)))
               lines begin end)
          (forward-line 0)
          (when (looking-at "^.*:range-begin *\"\\([^\"]+\\)\"")
            (setq begin (match-string-no-properties 1)))
          (when (looking-at "^.*:range-end *\"\\([^\"]+\\)\"")
            (setq end (match-string-no-properties 1)))
          (setq lines (endless/decide-line-range file begin end))
          (when lines
            (if (looking-at ".*:lines *\"\\([-0-9]+\\)\"")
                (replace-match lines :fixedcase :literal nil 1)
              (goto-char (line-end-position))
              (insert " :lines \"" lines "\""))))))))

;; * The background worker *
;; This is the guy that does most of the work.

(defun endless/decide-line-range (file begin end)
  "Visit FILE and decide which lines to include.
BEGIN and END are regexps which define the line range to use."
  (let (l r)
    (save-match-data
      (with-temp-buffer
        (insert-file file)
        (goto-char (point-min))
        (if (null begin)
            (setq l "")
          (search-forward-regexp begin)
          (setq l (line-number-at-pos (match-beginning 0))))
        (if (null end)
            (setq r "")
          (search-forward-regexp end)
          (setq r (1+ (line-number-at-pos (match-end 0)))))
        (format "%s-%s" l r)))))

;; Execute the `modi/org-include-img-from-pdf' function just before saving the file
(add-hook 'before-save-hook #'modi/org-include-img-from-pdf)
;; Execute the `modi/org-include-img-from-pdf' function before processing the
;; file for export
(add-hook 'org-export-before-processing-hook #'modi/org-include-img-from-pdf)

(defun modi/org-include-img-from-pdf (&rest ignore)
  "Convert the pdf files to image files.

Only looks at #HEADER: lines that have \":convertfrompdf t\".
This function does nothing if not in org-mode, so you can safely
add it to `before-save-hook'."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp
              "^\\s-*#\\+HEADER:.*\\s-:convertfrompdf\\s-+t"
              nil 'noerror)
        (let* (filenoext imgext imgfile pdffile cmd)
          ;; Keep on going on to the next line till it finds a line with
          ;; `[[FILE]]'
          (while (progn
                   (forward-line 1)
                   (not (looking-at "\\[\\[\\(.*\\)\\.\\(.*\\)\\]\\]"))))
          (when (looking-at "\\[\\[\\(.*\\)\\.\\(.*\\)\\]\\]")
            (setq filenoext (match-string-no-properties 1))
            (setq imgext (match-string-no-properties 2))
            (setq imgfile (expand-file-name (concat filenoext "." imgext)))
            (setq pdffile (expand-file-name (concat filenoext "." "pdf")))
            (setq cmd (concat "convert -density 96 -quality 85 "
                              pdffile " " imgfile))
            (when (file-newer-than-file-p pdffile imgfile)
              ;; This block is executed only if pdffile is newer than imgfile
              ;; or if imgfile does not exist
              ;; Source: https://www.gnu.org/software/emacs/manual/html_node/elisp/Testing-Accessibility.html
              (message "%s" cmd)
              (shell-command cmd))))))))

;; Update TODAY macro with current date
(add-hook 'org-export-before-processing-hook #'modi/org-update-TODAY-macro)
(defun modi/org-update-TODAY-macro (&rest ignore)
  "Update TODAY macro to hold string with current date."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp
              "^\\s-*#\\+MACRO:\\s-+TODAY"
              nil 'noerror)
        (forward-line 0)
        (when (looking-at ".*TODAY\\(.*\\)")
          (replace-match
           (concat " "
                   (format-time-string "%b %d %Y, %a" (current-time)))
           :fixedcase :literal nil 1))))))

;; Insert fancybox class to all images
;; Update the below variables as per instructions from
;; http://fancyapps.com/fancybox/#instructions
(setq modi/org-html-fancybox-jquery-library "
#+HTML_HEAD: <!-- Add jQuery library -->
#+HTML_HEAD: <script type=\"text/javascript\" src=\"http://code.jquery.com/jquery-latest.min.js\"></script>
")
(setq modi/org-html-fancybox-html-header "
#+HTML_HEAD: <!-- Add fancyBox -->
#+HTML_HEAD: <link rel=\"stylesheet\" href=\"common/js/fancybox/source/jquery.fancybox.css?v=2.1.5\" type=\"text/css\" media=\"screen\" />
#+HTML_HEAD: <script type=\"text/javascript\" src=\"common/js/fancybox/source/jquery.fancybox.pack.js?v=2.1.5\"></script>
")
(setq modi/org-html-fancybox-html-body "
#+BEGIN_HTML
<!-- Source for fixing the issue of image disappearing about launch of fancybox.
     Using $(\"a.fancybox\").fancybox(); instead of $(\"fancybox\").fancybox();
     The issue is caused because org-mode assign class=\"fancybox\" to both <a> and
     <img> elements. Using \"a.fancybox\" limits the script to just the <a> element.
-->
<script type=\"text/javascript\">
	$(document).ready(function() {
		$(\"a.fancybox\").fancybox();
	});
</script>
#+END_HTML
")
(setq modi/org-html-fancybox-img-file-prefix-regexp "\\(file\\|http\\|https\\)")
(setq modi/org-html-fancybox-img-file-regexp "\\(png\\|jpg\\|svg\\)")
(setq modi/org-html-fancybox-force-inline-img t)
(setq modi/org-html-fancybox-img-highrez-suffix "")
(setq modi/org-html-fancybox-img-thumb-suffix "")
(add-hook 'org-export-before-processing-hook #'modi/org-html-add-fancybox)
;; (add-hook 'before-save-hook #'modi/org-html-add-fancybox)
(remove-hook 'before-save-hook #'modi/org-html-add-fancybox)
(defun modi/org-html-add-fancybox (&rest ignore)
  "Update TODAY macro to hold string with current date and time."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (let* ((enable-fancybox nil))
      (save-excursion
        (goto-char (point-min))
        (while (search-forward-regexp
                "^\\s-*#\\+OPTIONS:.*fancybox:\\s-*t"
                nil 'noerror)
          (forward-line 1)
          ;; Insert jQuery library
          (when (bound-and-true-p modi/org-html-fancybox-jquery-library)
            (insert modi/org-html-fancybox-jquery-library))
          (forward-line 1)
          ;; Insert paths to fancybox css and js
          (when (bound-and-true-p modi/org-html-fancybox-html-header)
            (insert modi/org-html-fancybox-html-header))
          ;; Insert fancybox script in the html body
          (when (bound-and-true-p modi/org-html-fancybox-html-body)
            (insert modi/org-html-fancybox-html-body))
          (setq enable-fancybox t))
        ;; (message "Fancybox status: %s" enable-fancybox)
        (when enable-fancybox
          ;; Go back to top of the buffer
          (goto-char (point-min))
          ;; Search for [[FILE.png]] or [[FILE.jpg]] or
          ;;         or [[FILE.png][FILE.png]] or [[FILE.jpg][FILE.jpg]]
          (while (search-forward-regexp
                  (concat "^\\s-*\\[\\["
                          "\\(" modi/org-html-fancybox-img-file-prefix-regexp ":\\)*" ;; 1=file: 2=file
                          "\\(.*?\\)" ;; 3=img-highrez
                          "\\." modi/org-html-fancybox-img-file-regexp ;; 4=img-highrez-ext
                          "\\]\\s-*\\[*"
                          "\\(" modi/org-html-fancybox-img-file-prefix-regexp ":\\)*" ;; 5=file: 6=file
                          "\\(.*?\\)" ;; 7=img-thumb
                          "\\.*" modi/org-html-fancybox-img-file-regexp "*" ;; 8=img-thumb-ext
                          "\\]*"
                          "\\]")
                  nil 'noerror)
            (let* (file-prefix1 img-highrez img-highrez-ext
                                file-prefix2 img-thumb img-thumb-ext)
              (setq file-prefix1    (match-string-no-properties 1))
              (setq img-highrez     (match-string-no-properties 3))
              (setq img-highrez-ext (match-string-no-properties 4))
              (setq file-prefix2    (match-string-no-properties 5))
              (setq img-thumb       (match-string-no-properties 7))
              (setq img-thumb-ext   (match-string-no-properties 8))

              ;; (message "File prefixes: prefix 1: %s %s prefix 2: %s %s"
              ;;          file-prefix1 (string= file-prefix1 "")
              ;;          file-prefix2 (string= file-prefix2 ""))
              ;; (message "img-highrez: %s %s" img-highrez (string= img-highrez ""))
              ;; (message "img-highrez-ext: %s %s" img-highrez-ext (string= img-highrez-ext ""))
              ;; (message "img-thumb: %s %s" img-thumb (string= img-thumb ""))
              ;; (message "img-thumb-ext: %s %s %s"
              ;;          img-thumb-ext
              ;;          (string= img-thumb-ext "")
              ;;          (not (boundp 'img-thumb-ext)))
              ;; (message "%s %s %s %s %s %s %s %s \n"
              ;;          (match-string-no-properties 1)
              ;;          (match-string-no-properties 2)
              ;;          (match-string-no-properties 3)
              ;;          (match-string-no-properties 4)
              ;;          (match-string-no-properties 5)
              ;;          (match-string-no-properties 6)
              ;;          (match-string-no-properties 7)
              ;;          (match-string-no-properties 8))

              ;; If inserted image is of the style [[FILE.png]] or [[FILE.jpg]],
              ;; auto-populate the 'description' portion of image link so
              ;; that it translates to a hyper-linked inline image in HTML
              ;; NOTE: If the description part of the image link does not have
              ;; the a prefix like file:, the HTML export will show only a
              ;; hyper-linked image path instead of a hyper-linked inline image.
              ;; Source: http://orgmode.org/manual/Images-in-HTML-export.html
              (when (and (string= img-thumb "")
                         (string= img-thumb-ext nil))
                ;; (message "Here1")
                (setq img-thumb     img-highrez)
                (setq img-thumb-ext img-highrez-ext)
                (if (string= file-prefix1 nil)
                    (when modi/org-html-fancybox-force-inline-img
                      (setq file-prefix2 "file:"))
                  ;; set file-prefix2 equal to file-prefix1 if file-prefix1 is
                  ;; non-nil
                  (setq file-prefix2 file-prefix1))
                (forward-line 0)
                (when (looking-at ".*\\[\\[\\(.*\\)\\]\\]")
                  (replace-match (concat file-prefix1
                                         img-highrez
                                         modi/org-html-fancybox-img-highrez-suffix
                                         "." img-highrez-ext
                                         "]["
                                         file-prefix2
                                         img-thumb
                                         modi/org-html-fancybox-img-thumb-suffix
                                         "." img-thumb-ext)
                                 :fixedcase :literal nil 1)))
              ;; If the image link is of the type [[FILE.ext][FILE.ext]] and
              ;; if inline image option is forced, convert the image link to
              ;; [[FILE.ext][file:FILE.ext]]
              (when (and modi/org-html-fancybox-force-inline-img
                         (not (string= img-thumb ""))
                         (not (string= img-thumb-ext nil))
                         (string= file-prefix2 nil))
                (setq file-prefix2 "file:")
                (forward-line 0)
                (when (looking-at ".*\\[\\(.*\\)\\]\\]")
                  (replace-match (concat file-prefix2 img-highrez
                                         "." img-highrez-ext)
                                 :fixedcase :literal nil 1)))
              (forward-line 0)
              (open-line 1)
              (insert "#+ATTR_HTML: :class fancybox")
              (forward-line 2))))))))


;; Fix the issue of fci-mode and html export
(require 'ox-html)
(defun org-html-fontify-code (code lang)
  "Color CODE with htmlize library.
CODE is a string representing the source code to colorize.  LANG
is the language used for CODE, as a string, or nil."
  (when code
    (cond
     ;; Case 1: No lang.  Possibly an example block.
     ((not lang)
      ;; Simple transcoding.
      (org-html-encode-plain-text code))
     ;; Case 2: No htmlize or an inferior version of htmlize
     ((not (and (require 'htmlize nil t) (fboundp 'htmlize-region-for-paste)))
      ;; Emit a warning.
      (message "Cannot fontify src block (htmlize.el >= 1.34 required)")
      ;; Simple transcoding.
      (org-html-encode-plain-text code))
     (t
      ;; Map language
      (setq lang (or (assoc-default lang org-src-lang-modes) lang))
      (let* ((lang-mode (and lang (intern (format "%s-mode" lang)))))
	(cond
	 ;; Case 1: Language is not associated with any Emacs mode
	 ((not (functionp lang-mode))
	  ;; Simple transcoding.
	  (org-html-encode-plain-text code))
	 ;; Case 2: Default.  Fontify code.
	 (t
	  ;; htmlize
	  (setq code (with-temp-buffer
		       ;; Switch to language-specific mode.
		       (funcall lang-mode)
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                       (when (require 'fill-column-indicator nil 'noerror)
                         (fci-mode -1))
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		       (insert code)
		       ;; Fontify buffer.
		       (font-lock-fontify-buffer)
		       ;; Remove formatting on newline characters.
		       (save-excursion
			 (let ((beg (point-min))
			       (end (point-max)))
			   (goto-char beg)
			   (while (progn (end-of-line) (< (point) end))
			     (put-text-property (point) (1+ (point)) 'face nil)
			     (forward-char 1))))
		       (org-src-mode)
		       (set-buffer-modified-p nil)
		       ;; Htmlize region.
		       (org-html-htmlize-region-for-paste
			(point-min) (point-max))))
	  ;; Strip any enclosing <pre></pre> tags.
	  (let* ((beg (and (string-match "\\`<pre[^>]*>\n*" code) (match-end 0)))
		 (end (and beg (string-match "</pre>\\'" code))))
	    (if (and beg end) (substring code beg end) code)))))))))

;; Remove HTML tags from the title string; otherwise the tags show up
;; verbatim in browser tabs3
(defun org-html--build-meta-info (info)
  "Return meta tags for exported document.
INFO is a plist used as a communication channel."
  (let ((protect-string
	 (lambda (str)
	   (replace-regexp-in-string
	    "\"" "&quot;" (org-html-encode-plain-text str))))
	(title (org-export-data (plist-get info :title) info))
	(author (and (plist-get info :with-author)
		     (let ((auth (plist-get info :author)))
		       (and auth
			    ;; Return raw Org syntax, skipping non
			    ;; exportable objects.
			    (org-element-interpret-data
			     (org-element-map auth
				 (cons 'plain-text org-element-all-objects)
			       'identity info))))))
	(description (plist-get info :description))
	(keywords (plist-get info :keywords))
	(charset (or (and org-html-coding-system
			  (fboundp 'coding-system-get)
			  (coding-system-get org-html-coding-system
					     'mime-charset))
		     "iso-8859-1")))
    (concat
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; (format "<title>%s</title>\n" title) ;; ORIGINAL
     ;; Remove HTML tags from `title' string
     (format "<title>%s</title>\n"
             (replace-regexp-in-string ".*\\(<.*>\\).*" ""
                                       title :fixedcase :literal 1))
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     (when (plist-get info :time-stamp-file)
       (format-time-string
        (concat "<!-- " org-html-metadata-timestamp-format " -->\n")))
     (format
      (if (org-html-html5-p info)
	  (org-html-close-tag "meta" " charset=\"%s\"" info)
	(org-html-close-tag
	 "meta" " http-equiv=\"Content-Type\" content=\"text/html;charset=%s\""
	 info))
      charset) "\n"
      (org-html-close-tag "meta" " name=\"generator\" content=\"Org-mode\"" info)
      "\n"
      (and (org-string-nw-p author)
           (concat
            (org-html-close-tag "meta"
                                (format " name=\"author\" content=\"%s\""
                                        (funcall protect-string author))
                                info)
            "\n"))
      (and (org-string-nw-p description)
           (concat
            (org-html-close-tag "meta"
                                (format " name=\"description\" content=\"%s\"\n"
                                        (funcall protect-string description))
                                info)
            "\n"))
      (and (org-string-nw-p keywords)
           (concat
            (org-html-close-tag "meta"
                                (format " name=\"keywords\" content=\"%s\""
                                        (funcall protect-string keywords))
                                info)
            "\n")))))

;; Rename the use of "Listings" term in HTML exports
(defun org-html-list-of-listings (info)
  "Build a list of listings.
INFO is a plist used as a communication channel.  Return the list
of listings as a string, or nil if it is empty."
  (let ((lol-entries (org-export-collect-listings info)))
    (when lol-entries
      (concat "<div id=\"list-of-listings\">\n"
	      (format "<h%d>%s</h%d>\n"
		      org-html-toplevel-hlevel
		      (org-html--translate "Code Snippets" info)
		      org-html-toplevel-hlevel)
	      "<div id=\"text-list-of-listings\">\n<ul>\n"
	      (let ((count 0)
		    (initial-fmt (format "<span class=\"listing-number\">%s</span>"
					 (org-html--translate "Code Snippet %d:" info))))
		(mapconcat
		 (lambda (entry)
		   (let ((label (org-element-property :name entry))
			 (title (org-trim
				 (org-export-data
				  (or (org-export-get-caption entry t)
				      (org-export-get-caption entry))
				  info))))
		     (concat
		      "<li>"
		      (if (not label)
			  (concat (format initial-fmt (incf count)) " " title)
			(format "<a href=\"#%s\">%s %s</a>"
				(org-export-solidify-link-text label)
				(format initial-fmt (incf count))
				title))
		      "</li>")))
		 lol-entries "\n"))
	      "\n</ul>\n</div>\n</div>"))))

;; Center align the tables when exporting to HTML
;; Note: This aligns the whole table, not the table columns
(setq org-html-table-default-attributes
      '(:border "2"
                :cellspacing "0"
                :cellpadding "6"
                :rules "groups"
                :frame "hsides"
                :align "center"
                :class "table-striped")) ;; this class requires bootstrap.css ( http://getbootstrap.com )
      ;; '(:border "2" :cellspacing "0" :cellpadding "6" :rules "groups" :frame "hsides"))

;; Customize the HTML postamble
(setq org-html-postamble t) ;; default value = 'auto
(setq org-html-postamble-format
      '(("en" "Exported using <div style=\"display: inline\" class=\"creator\">%c</div> on <div style=\"display: inline\"class=\"date\">%d</div> by %e.")))

;; (setq org-html-htmlize-output-type 'inline-css) ; default
(setq org-html-htmlize-output-type 'css)
;; (setq org-html-htmlize-font-prefix "") ; default
(setq org-html-htmlize-font-prefix "org-")

;; Implementing Markdown style link IDs in org-mode
;; Source: http://emacs.stackexchange.com/questions/594/how-to-implement-markdown-style-link-ids-in-org-mode
(org-add-link-type "linkid" 'endless/open-linkid-link 'endless/export-linkid-link)

(defun endless/open-linkid-link (path)
  "Follow an LINKID link to PATH."
  (browse-url (endless/find-linkid-link path)))

(defun endless/export-linkid-link (path desc format)
  "Create the export version of an LINKID link specified by LINK and DESC.
FORMATs understood are 'latex and 'html."
  (setq path (endless/find-linkid-link path))
  (cond
   ((eq format 'html)  (format "<a href=\"%s\">%s</a>" path desc))
   ((eq format 'latex) (format "\\href{%s}{%s}" path desc))
   ((eq format 'ascii) (format "[%s](%s)" desc path))
   (t desc)))

(defun endless/find-linkid-link (linkid &optional noerror)
  "Find \"#+LINK-ID: LINKID\" in current buffer and return the link.
Unless NOERROR is non-nil, throw an error if link not found."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (when (search-forward-regexp
             (format "^#\\+LINK-ID: \\b%s\\b +\\(.*\\) *$" linkid)
             nil noerror)
        (match-string-no-properties 1)))))

;; Patched `org-ascii-link' function from `org-ascii.el'
;; Supports LINK TYPES added using `org-add-link-type' to ascii exports as well.
(setq org-ascii-links-to-notes nil)
(defun org-ascii-link (link desc info)
  "Transcode a LINK object from Org to ASCII.

DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information."
  (let* ((raw-link (org-element-property :raw-link link))
         (type (org-element-property :type link))
         (raw-path (replace-regexp-in-string
                    "%" "\\%" (org-element-property :path link) nil t))
         (path (cond
                ((member type '("http" "https" "ftp" "mailto"))
                 (concat type ":" raw-path))
                ((and (string= type "file") (file-name-absolute-p raw-path))
                 (concat "file:" raw-path))
                (t raw-path))))
    (cond
     ((string= type "coderef")
      (let ((ref (org-element-property :path link)))
	(format (org-export-get-coderef-format ref desc)
		(org-export-resolve-coderef ref info))))
     ;; Do not apply a special syntax on radio links.  Though, use
     ;; transcoded target's contents as output.
     ((string= type "radio") desc)
     ;; Do not apply a special syntax on fuzzy links pointing to
     ;; targets.
     ((string= type "fuzzy")
      (let ((destination (org-export-resolve-fuzzy-link link info)))
	(if (org-string-nw-p desc) desc
	  (when destination
	    (let ((number
		   (org-export-get-ordinal
		    destination info nil 'org-ascii--has-caption-p)))
	      (when number
		(if (atom number) (number-to-string number)
		  (mapconcat 'number-to-string number "."))))))))
     ;; Link type is handled by a special function.
     ((functionp (setq protocol (nth 2 (assoc type org-link-protocols))))
      (funcall protocol (org-link-unescape path) desc 'ascii))
     (t
      (if (not (org-string-nw-p desc))
          (format "[%s]" raw-link)
	(concat
	 (format "[%s]" desc)
	 (unless org-ascii-links-to-notes (format " (%s)" raw-link))))))))

;; Source: http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
;; Pressing `C-x C-s' while editing org source code blocks saves and exits
;; the edit.
(eval-after-load 'org-src
  '(define-key org-src-mode-map
     "\C-x\C-s" #'org-edit-src-exit))

;; ;; Enable org export to odt (OpenDocument Text)
;; ;; It is disabled by default in org 8.x
;; ;; The .odt files can be opened directly in MS Word.
;; ;; http://stackoverflow.com/a/22990257/1219634
;; (eval-after-load "org"
;;   '(require 'ox-odt nil t))


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
