;; Time-stamp: <2020-09-03 11:19:35 kmodi>
;; Hi-lock: (("\\(^;\\{3,\\}\\)\\( *.*\\)" (1 'org-hide prepend) (2 '(:inherit org-level-1 :height 1.3 :weight bold :overline t :underline t) prepend)))
;; Hi-Lock: end

;; Org Mode

;; Contents:
;;
;;  Org Variables
;;  Org Defuns
;;  Org File Apps
;;  Org Goto
;;  Easy Templates
;;  Bindings
;;  Org Export
;;    Org Export Customization
;;    Inbuilt Exporters
;;      ox-latex - LaTeX export
;;      ox-html - HTML export
;;      ox-beamer - Beamer export
;;      ox-odt - ODT, doc export
;;    External Exporters
;;      ox-reveal - Presentations using reveal.js
;;      ox-minutes - Meeting Minutes ASCII export
;;    Helper Packages for Export
;;      Convert included pdf to image
;;      Extract image from included .zip
;;  Org Src
;;  Org Capture
;;  Org Agenda
;;  Org Table
;;    Table Recalculation
;;    Table Field Marking
;;  Org Entities
;;  Org Babel
;;  Org Babel Tangle
;;    Diagrams
;;    Python
;;  Other Org Packages
;;    Org Tree Slide
;;    Org Cliplink
;;    Sticky Header Line
;;    Org Link Ref
;;    Htmlize Region→File
;;    Include Src lines
;;    Org TOC
;;  Provide
;;  Notes

(use-package org
  :preface
  (progn
    (defvar org-dev-lisp-directory (when modi/default-share-directory
                                     (let* ((dir-1 (file-name-as-directory (expand-file-name "emacs" modi/default-share-directory)))
                                            (dir-2 (file-name-as-directory (expand-file-name "site-lisp" dir-1)))
                                            (dir (file-name-as-directory (expand-file-name "org" dir-2))))
                                       dir))
      "Directory containing lisp files for dev version of Org.

This value must match the `lispdir' variable in the Org local.mk.
By default the value is \"$prefix/emacs/site-lisp/org\", where
`prefix' must match that in local.mk too.")

    (defvar org-dev-info-directory (when modi/default-share-directory
                                     (let* ((dir-1 (file-name-as-directory (expand-file-name "org" modi/default-share-directory)))
                                            (dir (file-name-as-directory (expand-file-name "info" dir-1))))
                                       dir))
      "Directory containing Info manual file for dev version of Org.

This value must match the `infodir' variable in the Org local.mk.")

    (when (and org-dev-lisp-directory
               org-dev-info-directory)
      (with-eval-after-load 'package
        ;; If `modi/org-version-select' is *not* `emacs', remove the Emacs
        ;; version of Org from the `load-path'.
        (unless (eq modi/org-version-select 'emacs)
          ;; Remove Org that ships with Emacs from the `load-path'.
          (let ((default-org-path (expand-file-name "org" modi/default-lisp-directory)))
            (setq load-path (delete default-org-path load-path))))

        (>=e "25.0" ;`directory-files-recursively' is not available in older emacsen
            ;; If `modi/org-version-select' is *not* `elpa', remove the Elpa
            ;; version of Org from the `load-path'.
            (unless (eq modi/org-version-select 'elpa)
              (dolist (org-elpa-install-path (directory-files-recursively
                                              package-user-dir
                                              "\\`org\\(-plus-contrib\\)*-[0-9.]+\\'"
                                              :include-directories))
                (setq load-path (delete org-elpa-install-path load-path))
                ;; Also ensure that the associated path is removed from Info
                ;; search list.
                (setq Info-directory-list (delete org-elpa-install-path Info-directory-list)))))

        (let ((dev-org-path (directory-file-name org-dev-lisp-directory))
              (dev-org-info (directory-file-name org-dev-info-directory)))
          (if (eq modi/org-version-select 'dev)
              (progn
                (add-to-list 'load-path dev-org-path)
                ;; It's possible that `org-dev-info-directory' is set to an
                ;; unconventional value, in which case, it will not be
                ;; automatically added to `Info-directory-alist'. So to ensure
                ;; that the correct Org Info is used, add it to
                ;; `Info-directory-alist' manually.
                (add-to-list 'Info-directory-list dev-org-info))
            ;; If `modi/org-version-select' is *not* `dev', remove the
            ;; development version of Org from the `load-path'.
            (setq load-path (delete dev-org-path load-path))
            (with-eval-after-load 'info
              ;; Also ensure that the associated path is removed from Info search
              ;; list.
              (setq Info-directory-list (delete dev-org-info Info-directory-list)))))))

    ;; Modules that should always be loaded together with org.el.
    ;; `org-modules' default:
    ;; Org 9.3 and older: '(org-w3m org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail)
    ;; Org 9.4 and newer: '(ol-w3m ol-bbdb ol-bibtex ol-docview ol-gnus ol-info ol-irc ol-mhe ol-rmail ol-eww)
    (if (eq modi/org-version-select 'dev)
        (setq org-modules '(ol-info))
      (setq org-modules '(org-info)))

    ;; Set my default org-export backends. This variable needs to be set before
    ;; org.el is loaded.
    (setq org-export-backends '(ascii html latex))
    ;; Do not open links of mouse left clicks.
    ;; Default behavior caused inline images in Org buffers to pop up in their
    ;; own buffers when left clicked on by mistake. I can still intentionally
    ;; open links and such images in new buffers by doing C-c C-o.
    (setq org-mouse-1-follows-link nil))
  :mode ("\\.org\\'" . org-mode)
  :config
  (progn
;;; Org Variables
    (setq org-src-fontify-natively t)   ;Fontify code in code blocks
    ;; Display entities like \tilde, \alpha, etc in UTF-8 characters
    (setq org-pretty-entities t)
    ;; Render subscripts and superscripts in Org buffers
    (setq org-pretty-entities-include-sub-superscripts t)

    ;; Allow _ and ^ characters to sub/super-script strings but only when
    ;; string is wrapped in braces
    (setq org-use-sub-superscripts '{}) ;In-buffer rendering

    ;; Single key command execution when at beginning of a headline
    (setq org-use-speed-commands t)     ;? speed-key opens Speed Keys help
    (setq org-speed-commands-user '(("m" . org-mark-subtree)))

    (setq org-hide-leading-stars  t)
    ;; Prevent auto insertion of blank lines before headings and list items
    (setq org-blank-before-new-entry '((heading)
                                       (plain-list-item)))

    ;; fold / overview  - collapse everything, show only level 1 headlines
    ;; content          - show only headlines
    ;; nofold / showall - expand all headlines except the ones with :archive:
    ;;                    tag and property drawers
    ;; showeverything   - same as above but without exceptions
    (setq org-startup-folded 'showall)

    ;; https://orgmode.org/manual/Clean-view.html
    (setq org-startup-indented t)       ;Enable `org-indent-mode' on Org startup
    (with-eval-after-load 'org-indent
      (setq org-indent-indentation-per-level 1)) ;Default = 2

    (setq org-log-done 'time) ;Insert only timestamp when closing an Org TODO item
    ;; (setq org-log-done 'note) ;Insert timestamp and note when closing an Org TODO item
    ;; https://orgmode.org/manual/Closing-items.html
    (setq org-todo-keywords '((sequence "TODO" "SOMEDAY" "CANCELED" "DONE")))
    (setq org-todo-keyword-faces
          '(("TODO"     . org-todo)
            ("SOMEDAY"  . (:foreground "black" :background "#FFEF9F"))
            ("CANCELED" . (:foreground "#94BFF3" :weight bold :strike-through t))
            ("DONE"     . (:foreground "black" :background "#91ba31"))))
    ;; Block entries from changing state to DONE while they have children
    ;; that are not DONE - https://orgmode.org/manual/TODO-dependencies.html
    (setq org-enforce-todo-dependencies t)

    ;; http://emacs.stackexchange.com/a/17513/115
    (setq org-special-ctrl-a/e '(t   ;For C-a. Possible values: nil, t, 'reverse
                                 . t)) ;For C-e. Possible values: nil, t, 'reverse

    (setq org-catch-invisible-edits 'smart) ;http://emacs.stackexchange.com/a/2091/115

    ;; Number of empty lines needed to keep an empty line between collapsed trees.
    ;; If you leave an empty line between the end of a subtree and the following
    ;; headline, this empty line is hidden when the subtree is folded.
    ;; Org-mode will leave (exactly) one empty line visible if the number of
    ;; empty lines is equal or larger to the number given in this variable.
    (setq org-cycle-separator-lines 2)  ;Default = 2

    (with-eval-after-load 'org-footnote
      ;; Prevent renumbering/sorting footnotes when a footnote is added/removed.
      ;; Doing so would create a big diff in an Org file containing lot of
      ;; footnotes even if only one footnote was added/removed.
      (setq org-footnote-auto-adjust nil)) ;`'sort' - only sort
                                        ;`'renumber' - only renumber
                                        ;`t' - sort and renumber
                                        ;`nil' - do nothing (default)

    ;; ;; The default value of `org-highlight-links' contains `plain' too.
    ;; ;; - "plain" links are links in normal text, no whitespace, like http://foo.com.
    ;; ;; Here's why I remove "plain" from this list:
    ;; ;; - If I have two bracketed links next to each-other in org-mode (like below),
    ;; ;;     [[http://google.com][1]],[[https://orgmode.org][2]]
    ;; ;; - and, if `plain' is in the `org-highlight-links' list,
    ;; ;;  the "," in there will also be highlighted as a link. So it would look
    ;; ;;  as if the "1,2" string pointed to a single link!
    ;; ;;    But without `plain' as part of this list, the "1" and "2" strings will
    ;; ;; look like separate links (as should be the case), as the "," will not be
    ;; ;; highlighted as a link.
    ;; (setq org-highlight-links (delete 'plain org-highlight-links))
    ;; Sat May 27 11:55:43 EDT 2017 - kmodi
    ;; Above workaround is not needed after this fix in Org master:
    ;;   https://code.orgmode.org/bzg/org-mode/commit/2d29269bb1b9af08011e091913798b6598e4b156

    ;; `org-default-notes-file' is used as a fall back file for org-capture.el.
    (setq org-default-notes-file (expand-file-name "notes.org" org-directory))

    (defvar modi/one-org-agenda-file (expand-file-name "agenda.files" org-directory)
      "One file to contain a list of all Org agenda files.")
    (setq org-agenda-files modi/one-org-agenda-file)
    (unless (file-exists-p modi/one-org-agenda-file)
      ;; http://stackoverflow.com/a/14072295/1219634
      ;; touch `modi/one-org-agenda-file'
      (write-region "" :ignore modi/one-org-agenda-file))

    ;; Allow multiple line Org emphasis markup
    ;; http://emacs.stackexchange.com/a/13828/115
    (setcar (nthcdr 4 org-emphasis-regexp-components) 20) ;Up to 20 lines, default is just 1
    ;; Below is needed to apply the modified `org-emphasis-regexp-components'
    ;; settings from above.
    (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

;;; Org Defuns
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
    ;; Tue Aug 29 11:35:28 EDT 2017 - kmodi
    ;; Stopped advising this today as I don't seem to find this useful.
    ;; (advice-add 'org-todo :around #'modi/org-first-convert-to-heading)

    (defun modi/org-return-no-indent (&optional n)
      "Make `org-return' repeat the number passed through the argument"
      (interactive "p")
      (dotimes (cnt n)
        (org-return nil)))

    ;; http://emacs.stackexchange.com/a/10712/115
    (defun modi/org-delete-link ()
      "Replace an Org link of the format [[LINK][DESCRIPTION]] with DESCRIPTION.
If the link is of the format [[LINK]], delete the whole Org link.

In both the cases, save the LINK to the kill-ring.

Execute this command while the point is on or after the hyper-linked Org link."
      (interactive)
      (when (derived-mode-p 'org-mode)
        (let ((search-invisible t) start end)
          (save-excursion
            (when (re-search-backward "\\[\\[" nil :noerror)
              (when (re-search-forward "\\[\\[\\(.*?\\)\\(\\]\\[.*?\\)*\\]\\]"
                                       nil :noerror)
                (setq start (match-beginning 0))
                (setq end   (match-end 0))
                (kill-new (match-string-no-properties 1)) ;Save link to kill-ring
                (replace-regexp "\\[\\[.*?\\(\\]\\[\\(.*?\\)\\)*\\]\\]" "\\2"
                                nil start end)))))))

    (defun modi/lower-case-org-keywords ()
      "Lower case Org keywords and block identifiers.

Example: \"#+TITLE\" -> \"#+title\"
         \"#+BEGIN_EXAMPLE\" -> \"#+begin_example\"

Inspiration:
https://code.orgmode.org/bzg/org-mode/commit/13424336a6f30c50952d291e7a82906c1210daf0."
      (interactive)
      (save-excursion
        (goto-char (point-min))
        (let ((case-fold-search nil)
              (count 0))
          ;; Match examples: "#+FOO bar", "#+FOO:", "=#+FOO=", "~#+FOO~",
          ;;                 "‘#+FOO’", "“#+FOO”", ",#+FOO bar",
          ;;                 "#+FOO_bar<eol>", "#+FOO<eol>".
          (while (re-search-forward "\\(?1:#\\+[A-Z_]+\\(?:_[[:alpha:]]+\\)*\\)\\(?:[ :=~’”]\\|$\\)" nil :noerror)
            (setq count (1+ count))
            (replace-match (downcase (match-string-no-properties 1)) :fixedcase nil nil 1))
          (message "Lower-cased %d matches" count))))

    (defun modi/org-in-any-block-p ()
      "Return non-nil if the point is in any Org block.

The Org block can be *any*: src, example, verse, etc., even any
Org Special block.

This function is heavily adapted from `org-between-regexps-p'."
      (save-match-data
        (let ((pos (point))
              (case-fold-search t)
              (block-begin-re "^[[:blank:]]*#\\+begin_\\(?1:.+?\\)\\(?: .*\\)*$")
              (limit-up (save-excursion (outline-previous-heading)))
              (limit-down (save-excursion (outline-next-heading)))
              beg end)
          (save-excursion
            ;; Point is on a block when on BLOCK-BEGIN-RE or if
            ;; BLOCK-BEGIN-RE can be found before it...
            (and (or (org-in-regexp block-begin-re)
                     (re-search-backward block-begin-re limit-up :noerror))
                 (setq beg (match-beginning 0))
                 ;; ... and BLOCK-END-RE after it...
                 (let ((block-end-re (concat "^[[:blank:]]*#\\+end_"
                                             (match-string-no-properties 1)
                                             "\\( .*\\)*$")))
                   (goto-char (match-end 0))
                   (re-search-forward block-end-re limit-down :noerror))
                 (> (setq end (match-end 0)) pos)
                 ;; ... without another BLOCK-BEGIN-RE in-between.
                 (goto-char (match-beginning 0))
                 (not (re-search-backward block-begin-re (1+ beg) :noerror))
                 ;; Return value.
                 (cons beg end))))))

    (defun modi/org-split-block ()
      "Sensibly split the current Org block at point.

(1) Point in-between a line

    #+begin_src emacs-lisp             #+begin_src emacs-lisp
    (message▮ \"one\")                   (message \"one\")
    (message \"two\")          -->       #+end_src
    #+end_src                          ▮
                                       #+begin_src emacs-lisp
                                       (message \"two\")
                                       #+end_src

(2) Point at EOL

    #+begin_src emacs-lisp             #+begin_src emacs-lisp
    (message \"one\")▮                   (message \"one\")
    (message \"two\")          -->       #+end_src
    #+end_src                          ▮
                                       #+begin_src emacs-lisp
                                       (message \"two\")
                                       #+end_src

(3) Point at BOL

    #+begin_src emacs-lisp             #+begin_src emacs-lisp
    (message \"one\")                    (message \"one\")
    ▮(message \"two\")          -->      #+end_src
    #+end_src                          ▮
                                       #+begin_src emacs-lisp
                                       (message \"two\")
                                       #+end_src
"
      (interactive)
      (if (modi/org-in-any-block-p)
          (save-match-data
            (save-restriction
              (widen)
              (let ((case-fold-search t)
                    (at-bol (bolp))
                    block-start
                    block-end)
                (save-excursion
                  (re-search-backward "^\\(?1:[[:blank:]]*#\\+begin_.+?\\)\\(?: .*\\)*$" nil nil 1)
                  (setq block-start (match-string-no-properties 0))
                  (setq block-end (replace-regexp-in-string
                                   "begin_" "end_" ;Replaces "begin_" with "end_", "BEGIN_" with "END_"
                                   (match-string-no-properties 1))))
                ;; Go to the end of current line, if not at the BOL
                (unless at-bol
                  (end-of-line 1))
                (insert (concat (if at-bol "" "\n")
                                block-end
                                "\n\n"
                                block-start
                                (if at-bol "\n" "")))
                ;; Go to the line before the inserted "#+begin_ .." line
                (beginning-of-line (if at-bol -1 0)))))
        (message "Point is not in an Org block")))

    (defun modi/org-meta-return (&optional arg)
      "Insert a new heading or wrap a region in a table.
Calls `org-insert-heading', `org-insert-item',
`org-table-wrap-region', or `modi/org-split-block' depending on
context.  When called with an argument, unconditionally call
`org-insert-heading'."
      (interactive "P")
      (org-check-before-invisible-edit 'insert)
      (or (run-hook-with-args-until-success 'org-metareturn-hook)
          (call-interactively (cond (arg #'org-insert-heading)
                                    ((org-at-table-p) #'org-table-wrap-region)
                                    ((org-in-item-p) #'org-insert-item)
                                    ((modi/org-in-any-block-p) #'modi/org-split-block)
                                    (t #'org-insert-heading)))))
    (advice-add 'org-meta-return :override #'modi/org-meta-return)

    ;; Make C-u C-return insert heading *at point* (not respecting content),
    ;; even when the point is directly after a list item.
    ;; Reason: http://lists.gnu.org/r/emacs-orgmode/2018-02/msg00368.html
    (defun modi/org-insert-heading-respect-content (&optional invisible-ok)
      "Insert heading with `org-insert-heading-respect-content' set to t.
With \\[universal-argument] prefix, insert Org heading directly at
point."
      (interactive)
      (let ((respect-content (unless current-prefix-arg
                               '(4))))
        (org-insert-heading respect-content invisible-ok)))
    (advice-add 'org-insert-heading-respect-content :override
                #'modi/org-insert-heading-respect-content)

    ;; Below helped be speed up eless.org tangling speed from ~4
    ;; seconds to ~2.5 seconds, and exporting time of nim.org when
    ;; down from 53 seconds to 39 seconds.
    ;; https://www.reddit.com/r/emacs/comments/9lpybf/a_fast_org_tangling_solution_using_nim/e7bettq/
    (defun modi/advice-org-tangle-and-export-boost (orig-fun &rest args)
      "Speed up Org tangling and exporting considerably.

- Prevent GC.
- Prevent Projectile hooks/advices from interfering with the find-files
  and file deletes."
      (let ((orig-gc-thresh gc-cons-threshold)
            (projectile-enabled (and (fboundp #'projectile-mode)
                                     projectile-mode))
            (t1 (current-time)))

        ;; Try to prevent GC.
        (setq gc-cons-threshold (* 200 1024 1024)) ;200 MB before garbage collection

        (when projectile-enabled
          ;; Disable projectile hooks and advices.
          (remove-hook 'find-file-hook #'projectile-find-file-hook-function)
          (advice-remove 'delete-file #'delete-file-projectile-remove-from-cache))

        (prog1
            (apply orig-fun args)
          ;; Re-add projectile mode hooks and advices.
          (when projectile-enabled
            (add-hook 'find-file-hook #'projectile-find-file-hook-function)
            (advice-add 'delete-file :before #'delete-file-projectile-remove-from-cache))

          (setq gc-cons-threshold orig-gc-thresh)
          (message "exec time: %S" (float-time (time-since t1))))))
    (dolist (fn '(org-babel-tangle org-export-to-file))
      (advice-add fn :around #'modi/advice-org-tangle-and-export-boost)
      ;; (advice-remove fn #'modi/advice-org-tangle-and-export-boost)
      )

;;; Org File Apps
    ;; Make firefox the default web browser for applications like viewing
    ;; an html file exported from Org ( C-c C-e h o )
    (when (executable-find "firefox")
      (add-to-list 'org-file-apps '("\\.x?html\\'" . "firefox %s")))
    ;; Change the default app for opening pdf files from org
    ;; http://stackoverflow.com/a/9116029/1219634
    (setcdr (assoc "\\.pdf\\'" org-file-apps) "acroread %s")

;;; Org Goto
    (defun modi/org-goto-override-bindings (&rest _)
      "Override the bindings set by `org-goto-map' function."
      (org-defkey org-goto-map "\C-p" #'outline-previous-visible-heading)
      (org-defkey org-goto-map "\C-n" #'outline-next-visible-heading)
      (org-defkey org-goto-map "\C-f" #'outline-forward-same-level)
      (org-defkey org-goto-map "\C-b" #'outline-backward-same-level)
      org-goto-map)
    (advice-add 'org-goto-map :after #'modi/org-goto-override-bindings)

;;; Easy Templates
    ;; https://orgmode.org/manual/Easy-Templates.html
    ;; http://oremacs.com/2015/03/07/hydra-org-templates
    ;; https://github.com/abo-abo/hydra/wiki/Org-mode-block-templates

    ;; Sun Nov 05 09:30:51 EST 2017 - kmodi
    ;; Copy of the old "Easy Templates" feature that was removed in
    ;; https://code.orgmode.org/bzg/org-mode/commit/c04e357f3d5d93484277a7e439847b1233b872bd
    (defconst org-easy-template-alist   ;Old `org-structure-template-alist'
      '(("s" "#+begin_src ?\n\n#+end_src")
        ("bd" "#+begin_description\n?\n#+end_description") ;Special block in `ox-hugo'
        ("bn" "#+begin_note\n?\n#+end_note") ;Special block in `ox-hugo'
        ("e" "#+begin_example\n?\n#+end_example")
        ("q" "#+begin_quote\n?\n#+end_quote")
        ("v" "#+begin_verse\n?\n#+end_verse")
        ("V" "#+begin_verbatim\n?\n#+end_verbatim")
        ("c" "#+begin_center\n?\n#+end_center")
        ("C" "#+begin_comment\n?\n#+end_comment")
        ("X" "#+begin_export ?\n\n#+end_export")
        ("l" "#+begin_export latex\n?\n#+end_export")
        ("L" "#+latex: ")
        ("h" "#+begin_export html\n?\n#+end_export")
        ("H" "#+html: ")
        ("a" "#+begin_export ascii\n?\n#+end_export")
        ("A" "#+ascii: ")
        ("i" "#+index: ?")
        ("I" "#+include: %file ?"))
      "Structure completion elements.
This is a list of abbreviation keys and values.  The value gets
inserted if you type `<' followed by one or more characters and
then press the completion key, usually `TAB'.  %file will be
replaced by a file name after prompting for the file using
completion.  The cursor will be placed at the position of the `?'
in the template.")

    (defun org-try-structure-completion ()
      "Try to complete a structure template before point.
This looks for strings like \"<e\" on an otherwise empty line and
expands them."
      (let ((l (buffer-substring (point-at-bol) (point)))
            a)
        (when (and (looking-at "[ \t]*$")
                   (string-match "^[ \t]*<\\([a-zA-Z]+\\)$" l)
                   (setq a (assoc (match-string 1 l) org-easy-template-alist)))
          (org-complete-expand-structure-template (+ -1 (point-at-bol)
                                                     (match-beginning 1))
                                                  a)
          t)))

    (defun org-complete-expand-structure-template (start cell)
      "Expand a structure template."
      (let ((rpl (nth 1 cell))
            (ind ""))
        (delete-region start (point))
        (when (string-match "\\`[ \t]*#\\+" rpl)
          (cond
           ((bolp))
           ((not (string-match "\\S-" (buffer-substring (point-at-bol) (point))))
            (setq ind (buffer-substring (point-at-bol) (point))))
           (t (newline))))
        (setq start (point))
        (when (string-match "%file" rpl)
          (setq rpl (replace-match
                     (concat
                      "\""
                      (save-match-data
                        (abbreviate-file-name (read-file-name "Include file: ")))
                      "\"")
                     t t rpl)))
        (setq rpl (mapconcat 'identity (split-string rpl "\n")
                             (concat "\n" ind)))
        (insert rpl)
        (when (re-search-backward "\\?" start t) (delete-char 1))))

    (defun modi/org-template-expand (str &optional arg)
      "Expand Org template based on STR.

STR is always prefixed with \"<\".  The string following that
\"<\" must match with the `car' of one of the elements in
`org-easy-template-alist' (examples: \"<e\", \"<s\").

If no region is selected, this function simply runs
`org-try-structure-completion' and does the template expansion
based on `org-easy-template-alist'.  If a region is selected, the
selected text is wrapped with that Org template.

If the \"#+begin_src\" block is inserted and ARG is a string
representing the source language, that source block is annotated
with that ARG.  If ARG is nil, point is returned to the end of
the \"#+begin_src\" line after the template insertion.

If the \"#+begin_export\" block is inserted and ARG is a string
representing the export backend, that export block is annotated
with that ARG.  If ARG is nil, point is returned to the end of
the \"#+begin_export\" line after the template insertion."
      (let* ((is-region? (use-region-p))
             (beg (if is-region?
                      (region-beginning)
                    (point)))
             ;; Copy marker for end so that if characters are added/removed
             ;; before the `end', the reference end point is updated (because of
             ;; being a marker).
             (end (when is-region?
                    (copy-marker (region-end) t)))
             content post-src-export column)

        (goto-char beg)

        ;; Save the indentation level of the content (if region is selected) or
        ;; the point (if region is not selected).
        (save-excursion
          (forward-line 0)            ;Go to BOL
          (when (looking-at "[[:blank:]]")
            (back-to-indentation)
            (setq column (current-indentation))))

        (when is-region?
          ;; Update `beg' if needed..
          ;; If `beg' is at BOL, update `beg' to be at the indentation.
          (when (and (bolp)
                     column)
            (back-to-indentation)
            (setq beg (point)))

          ;; Insert a newline if `beg' is *not* at BOL.
          ;; Example: You have ^abc$ where ^ is BOL and $ is EOL.
          ;;          "bc" is selected and pressing <e should result in:
          ;;            a
          ;;            #+begin_example
          ;;            bc
          ;;            #+end_example
          (unless (or (bolp)
                      (looking-back "^[[:blank:]]*"))
            (insert "\n")
            (when column
              (indent-to column))
            (setq beg (point)))

          ;; Update `end' if needed ..
          (goto-char end)
          (cond
           ((bolp)                      ;`end' is at BOL
            (skip-chars-backward " \n\t")
            (set-marker end (point)))
           ((and (not (bolp))           ;`end' is neither at BOL nor at EOL
                 (not (looking-at "[[:blank:]]*$")))
            ;; Insert a newline if `end' is neither at BOL nor EOL
            ;; Example: You have ^abc$ where ^ is bol and $ is eol.
            ;;          "a" is selected and pressing <e should result in:
            ;;            #+begin_example
            ;;            a
            ;;            #+end_example
            ;;            bc
            (insert "\n")
            (when column
              (indent-to column))
            (skip-chars-backward " \n\t")
            (set-marker end (point)))
           (t          ;`end' is either at EOL or looking at trailing whitespace
            ))
          ;; Now delete the content in the selected region and save it to
          ;; `content'.
          (setq content (delete-and-extract-region beg end))
          ;; Make the `end' marker point to nothing as its job is done.
          ;; http://lists.gnu.org/archive/html/emacs-orgmode/2017-09/msg00049.html
          ;; (elisp) Overview of Markers
          (set-marker end nil))

        ;; Insert the `str' required for template expansion (example: "<e").
        (insert str)
        (org-try-structure-completion)
        (when (let* ((case-fold-search t)) ;Ignore case
                (looking-back "^[[:blank:]]*#\\+begin_\\(src\\|export\\)[[:blank:]]+"))
          (cond
           ((stringp arg)
            (insert arg)
            (forward-line))
           ((and (null arg) ;If the language for the source block,
                 content)   ;or the backend for the export block is not specified
            (setq post-src-export (point))
            (forward-line))
           (t
            )))
        ;; At this point the cursor will be between the #+begin_.. and
        ;; #+end_.. lines.  Now also indent the point forward if needed.
        (when column
          (indent-to column))

        ;; Now if a region was selected, and `content' was saved from that,
        ;; paste it back in.
        (when content
          ;; A special case for verbatim blocks.. need to escape "*" and "#+"
          ;; with commas -- (org) Literal examples.
          ;; http://lists.gnu.org/archive/html/emacs-orgmode/2017-10/msg00349.html
          (when (save-excursion
                  (previous-line)
                  (forward-line 0)
                  (let* ((case-fold-search t)) ;Ignore case
                    (looking-at-p (concat "^[[:blank:]]*#\\+BEGIN_"
                                          (regexp-opt '("EXAMPLE" "EXPORT" "SRC"))))))
            (setq content (org-escape-code-in-string content)))
          (insert content)
          (deactivate-mark)
          (when post-src-export ;Case where user needs to specify the #+begin_src language,
            (goto-char post-src-export))))) ;or the #+begin_export backend.

    (defhydra hydra-org-template (:color blue
                                  :hint nil)
      "
org-template:  _c_enter        _s_rc          _e_xample           _v_erilog        _t_ext              _I_NCLUDE:
               _l_atex         _h_tml         _V_erse             _m_atlab         _L_aTeX:            _H_TML:
               _a_scii         _q_uote        _E_macs-lisp        _n_im            _i_ndex:            _A_SCII:
               _o_rg           _S_hell        _p_ython            e_X_port         [_bd_] description  [_bn_] note
"
      ("s" (modi/org-template-expand "<s")) ;#+begin_src ... #+end_src
      ("E" (modi/org-template-expand "<s" "emacs-lisp"))
      ("v" (modi/org-template-expand "<s" "systemverilog"))
      ("m" (modi/org-template-expand "<s" "matlab"))
      ("n" (modi/org-template-expand "<s" "nim"))
      ("o" (modi/org-template-expand "<s" "org"))
      ("S" (modi/org-template-expand "<s" "shell"))
      ("p" (modi/org-template-expand "<s" "python"))
      ("t" (modi/org-template-expand "<s" "text"))
      ("bd" (modi/org-template-expand "<bd")) ;#+begin_description ... #+end_description (Special block in `ox-hugo')
      ("bn" (modi/org-template-expand "<bn")) ;#+begin_note ... #+end_note (Special block in `ox-hugo')
      ("e" (modi/org-template-expand "<e")) ;#+begin_example ... #+end_example
      ("x" (modi/org-template-expand "<e")) ;#+begin_example ... #+end_example
      ("q" (modi/org-template-expand "<q")) ;#+begin_quote ... #+end_quote
      ("V" (modi/org-template-expand "<v")) ;#+begin_verse ... #+end_verse
      ("c" (modi/org-template-expand "<c")) ;#+begin_center ... #+end_center
      ("X" (modi/org-template-expand "<X")) ;#+begin_export ... #+end_export
      ("l" (modi/org-template-expand "<X" "latex")) ;#+begin_export latex ... #+end_export
      ("h" (modi/org-template-expand "<X" "html")) ;#+begin_export html ... #+end_export
      ("a" (modi/org-template-expand "<X" "ascii")) ;#+begin_export ascii ... #+end_export
      ("L" (modi/org-template-expand "<L")) ;#+latex:
      ("H" (modi/org-template-expand "<H")) ;#+html:
      ("A" (modi/org-template-expand "<A")) ;#+ascii:
      ("i" (modi/org-template-expand "<i")) ;#+index: line
      ("I" (modi/org-template-expand "<I")) ;#+include: line
      ("<" self-insert-command "<")
      ("Q" nil "quit"))

    (defun modi/org-template-maybe ()
      "Insert org-template if point is at the beginning of the
line, or if a region is selected.  Else call
`self-insert-command'."
      (interactive)
      (let ((is-region? (use-region-p)))
        (if (or is-region?
                (and (not is-region?)
                     (looking-back "^[[:blank:]]*")))
            (hydra-org-template/body)
          (self-insert-command 1))))

;;; Bindings
    (defun modi/reset-local-set-keys ()
      (local-unset-key (kbd "<f10>"))
      (local-unset-key (kbd "<s-f10>"))
      (local-unset-key (kbd "<S-f10>"))
      (local-unset-key (kbd "<C-f10>")))
    (add-hook 'org-mode-hook #'modi/reset-local-set-keys)

    (bind-keys
     :map org-mode-map
     ("C-m" . modi/org-return-no-indent)
     ("<" . modi/org-template-maybe)
     ("M-p" . org-previous-visible-heading)
     ("M-P" . org-backward-heading-same-level)
     ("M-n" . org-next-visible-heading)
     ("M-N" . org-forward-heading-same-level))

    ;; Bind the "org-table-*" command ONLY when the point is in an Org table.
    ;; http://emacs.stackexchange.com/a/22457/115
    (bind-keys
     :map org-mode-map
     :filter (org-at-table-p)
     ("C-c ?" . org-table-field-info)
     ("C-c SPC" . org-table-blank-field)
     ("C-c +" . org-table-sum)
     ("C-c =" . org-table-eval-formula)
     ("C-c `" . org-table-edit-field)
     ("C-#" . org-table-rotate-recalc-marks)
     ("C-c }" . org-table-toggle-coordinate-overlays)
     ("C-c {" . org-table-toggle-formula-debugger)
     ;; Add the <return> variant of bindings so that they work on
     ;; Emacs GUI too.
     ("S-<return>" . org-table-copy-down))

    ;; https://lists.gnu.org/r/emacs-orgmode/2019-01/msg00312.html
    ;; Add the <return> variant of bindings so that they work on Emacs
    ;; GUI too.
    (bind-keys
     :map org-mode-map
     ("M-S-<return>" . org-insert-todo-heading)
     ("ESC S-<return>" . org-insert-todo-heading)
     ("M-<return>" . org-meta-return)
     ("ESC <return>" . org-meta-return))

    (bind-keys
     :map modi-mode-map
     ("C-c a" . org-agenda)
     ("C-c c" . org-capture)
     ("C-c i" . org-store-link))))

;;; Org Export
(use-package ox
  :defer t
  :config
  (progn
    ;; Require wrapping braces to interpret _ and ^ as sub/super-script
    (setq org-export-with-sub-superscripts '{}) ;also #+options: ^:{}
    (setq org-export-with-smart-quotes t) ;also #+options: ':t

    (setq org-export-headline-levels 4)

;;;; Org Export Customization
    ;; Delete selected columns from Org tables before exporting
    ;; http://thread.gmane.org/gmane.emacs.orgmode/106497/focus=106683
    (defun mbrand/org-export-delete-commented-cols (back-end)
      "Delete columns $2 to $> marked as `<#>' on a row with `/' in $1.
If you want a non-empty column $1 to be deleted make it $2 by
inserting an empty column before and adding `/' in $1."
      (goto-char (point-min))
      (while (re-search-forward
              "^[ \t]*| +/ +|\\(.*|\\)? +\\(<#>\\) *|" nil :noerror)
        (goto-char (match-beginning 2))
        (org-table-delete-column)
        (beginning-of-line)))
    (add-hook 'org-export-before-processing-hook #'mbrand/org-export-delete-commented-cols)

    ;; http://lists.gnu.org/archive/html/emacs-orgmode/2016-09/msg00168.html
    (defun cpit/filter-begin-only (type)
      "Remove BEGIN_ONLY %s blocks whose %s doesn't equal TYPE.
For those that match, only remove the delimiters.

On the flip side, for BEGIN_EXCEPT %s blocks, remove those if %s equals TYPE. "
      (goto-char (point-min))
      (while (re-search-forward " *#\\+BEGIN_\\(ONLY\\|EXCEPT\\) +\\([a-z]+\\)\n"
                                nil :noerror)
        (let ((only-or-export (match-string-no-properties 1))
              (block-type (match-string-no-properties 2))
              (begin-from (match-beginning 0))
              (begin-to (match-end 0)))
          (re-search-forward (format " *#\\+END_%s +%s\n"
                                     only-or-export block-type))
          (let ((end-from (match-beginning 0))
                (end-to (match-end 0)))
            (if (or (and (string= "ONLY" only-or-export)
                         (string= type block-type))
                    (and (string= "EXCEPT" only-or-export)
                         (not (string= type block-type))))
                (progn              ;Keep the block,
                                        ;delete just the comment markers
                  (delete-region end-from end-to)
                  (delete-region begin-from begin-to))
              ;; Delete the block
              (message "Removing %s block" block-type)
              (delete-region begin-from end-to))))))
    (add-hook 'org-export-before-processing-hook #'cpit/filter-begin-only)

;;;; Inbuilt Exporters

    (defun modi/org-export-to-html-txt-pdf ()
      "Export the Org file to HTML, Ascii and PDF formats."
      (interactive)
      (org-html-export-to-html)
      (org-ascii-export-to-ascii)
      (org-latex-export-to-pdf))

;;;;; ox-latex - LaTeX export
    (use-package ox-latex
      :config
      (progn
        (defvar modi/ox-latex-use-minted t
          "Use `minted' package for listings.")

        (setq org-latex-compiler "xelatex") ;introduced in Org 9.0

        (setq org-latex-prefer-user-labels t) ;org-mode version 8.3+

        ;; ox-latex patches
        (load (expand-file-name
               "ox-latex-patches.el"
               (concat user-emacs-directory "elisp/patches/"))
              nil :nomessage)

        ;; Previewing latex fragments in Org mode
        ;; https://orgmode.org/worg/org-tutorials/org-latex-preview.html
        ;; (setq org-latex-create-formula-image-program 'dvipng) ;NOT Recommended
        (setq org-latex-create-formula-image-program 'imagemagick) ;Recommended

        ;; Controlling the order of loading certain packages w.r.t. `hyperref'
        ;; http://tex.stackexchange.com/a/1868/52678
        ;; ftp://ftp.ctan.org/tex-archive/macros/latex/contrib/hyperref/README.pdf
        ;; Remove the list element in `org-latex-default-packages-alist'.
        ;; that has '("hyperref" nil) as its cdr.
        ;; http://stackoverflow.com/a/9813211/1219634
        (setq org-latex-default-packages-alist
              (delq (rassoc '("hyperref" nil) org-latex-default-packages-alist)
                    org-latex-default-packages-alist))
        ;; `hyperref' will be added again later in `org-latex-packages-alist'
        ;; in the correct order.

        ;; The `org-latex-packages-alist' will output tex files with
        ;;   \usepackage[FIRST STRING IF NON-EMPTY]{SECOND STRING}
        ;; It is a list of cells of the format:
        ;;   ("options" "package" SNIPPET-FLAG COMPILERS)
        ;; If SNIPPET-FLAG is non-nil, the package also needs to be included
        ;; when compiling LaTeX snippets into images for inclusion into
        ;; non-LaTeX output (like when previewing latex fragments using the
        ;; "C-c C-x C-l" binding.
        ;; COMPILERS is a list of compilers that should include the package,
        ;; see `org-latex-compiler'.  If the document compiler is not in the
        ;; list, and the list is non-nil, the package will not be inserted
        ;; in the final document.

        (defconst modi/org-latex-packages-alist-pre-hyperref
          '(("letterpaper,margin=1.0in" "geometry")
            ;; Prevent an image from floating to a different location.
            ;; http://tex.stackexchange.com/a/8633/52678
            ("" "float")
            ;; % 0 paragraph indent, adds vertical space between paragraphs
            ;; http://en.wikibooks.org/wiki/LaTeX/Paragraph_Formatting
            ("" "parskip"))
          "Alist of packages that have to be loaded before `hyperref'
package is loaded.
ftp://ftp.ctan.org/tex-archive/macros/latex/contrib/hyperref/README.pdf ")

        (defconst modi/org-latex-packages-alist-post-hyperref
          '(;; Prevent tables/figures from one section to float into another section
            ;; http://tex.stackexchange.com/a/282/52678
            ("section" "placeins")
            ;; Graphics package for more complicated figures
            ("" "tikz")
            ("" "caption")
            ;;
            ;; Packages suggested to be added for previewing latex fragments
            ;; https://orgmode.org/worg/org-tutorials/org-latex-preview.html
            ("mathscr" "eucal")
            ("" "latexsym"))
          "Alist of packages that have to (or can be) loaded after `hyperref'
package is loaded.")

        ;; The "H" option (`float' package) prevents images from floating around.
        (setq org-latex-default-figure-position "H") ;figures are NOT floating
        ;; (setq org-latex-default-figure-position "htb") ;default - figures are floating

        ;; `hyperref' package setup
        (setq org-latex-hyperref-template
              (concat "\\hypersetup{\n"
                      "pdfauthor={%a},\n"
                      "pdftitle={%t},\n"
                      "pdfkeywords={%k},\n"
                      "pdfsubject={%d},\n"
                      "pdfcreator={%c},\n"
                      "pdflang={%L},\n"
                      ;; Get rid of the red boxes drawn around the links
                      "colorlinks,\n"
                      "citecolor=black,\n"
                      "filecolor=black,\n"
                      "linkcolor=blue,\n"
                      "urlcolor=blue\n"
                      "}"))

        (if modi/ox-latex-use-minted
            ;; using minted
            ;; https://github.com/gpoore/minted
            (progn
              (setq org-latex-listings 'minted) ;default nil
              ;; The default value of the `minted' package option `cachedir'
              ;; is "_minted-\jobname". That clutters the working dirs with
              ;; _minted* dirs. So instead create them in temp folders.
              (defvar latex-minted-cachedir
                (file-name-as-directory
                 (expand-file-name ".minted/\\jobname" modi/temporary-file-directory)))
              ;; `minted' package needed to be loaded AFTER `hyperref'.
              ;; http://tex.stackexchange.com/a/19586/52678
              (add-to-list 'modi/org-latex-packages-alist-post-hyperref
                           `(,(concat "cachedir=" ;options
                                      latex-minted-cachedir)
                             "minted" ;package
                             ;; If `org-latex-create-formula-image-program'
                             ;; is set to `dvipng', minted package cannot be
                             ;; used to show latex previews.
                             ,(not (eq org-latex-create-formula-image-program 'dvipng)))) ;snippet-flag

              ;; minted package options (applied to embedded source codes)
              (setq org-latex-minted-options
                    '(("linenos")
                      ("numbersep" "5pt")
                      ("frame"     "none") ;box frame is created by `mdframed' package
                      ("framesep"  "2mm")
                      ;; minted 2.0+ required for `breaklines'
                      ("breaklines"))) ;line wrapping within code blocks
              (when (equal org-latex-compiler "pdflatex")
                (add-to-list 'org-latex-minted-options '(("fontfamily"  "zi4")))))
          ;; not using minted
          (progn
            ;; Commented out below because it clashes with `placeins' package
            ;; (add-to-list 'modi/org-latex-packages-alist-post-hyperref '("" "color"))
            (add-to-list 'modi/org-latex-packages-alist-post-hyperref '("" "listings"))))

        (setq org-latex-packages-alist
              (append modi/org-latex-packages-alist-pre-hyperref
                      '(("" "hyperref" nil))
                      modi/org-latex-packages-alist-post-hyperref))

        ;; `-shell-escape' is required when using the `minted' package

        ;; https://orgmode.org/worg/org-faq.html#using-xelatex-for-pdf-export
        ;; latexmk runs pdflatex/xelatex (whatever is specified) multiple times
        ;; automatically to resolve the cross-references.
        ;; (setq org-latex-pdf-process '("latexmk -xelatex -quiet -shell-escape -f %f"))

        ;; Below value of `org-latex-pdf-process' with %latex will work in Org 9.0+
        ;; (setq org-latex-pdf-process
        ;;       '("%latex -interaction nonstopmode -shell-escape -output-directory %o %f"
        ;;         "%latex -interaction nonstopmode -shell-escape -output-directory %o %f"
        ;;         "%latex -interaction nonstopmode -shell-escape -output-directory %o %f"))

        ;; Run xelatex multiple times to get the cross-references right
        (setq org-latex-pdf-process '("xelatex -shell-escape %f"
                                      "xelatex -shell-escape %f"
                                      "xelatex -shell-escape %f"))

        ;; Override `org-latex-format-headline-default-function' definition
        ;; so that the TODO keyword in TODO marked headings is exported in
        ;; bold red.
        (defun org-latex-format-headline-default-function
            (todo _todo-type priority text tags info)
          "Default format function for a headline.
See `org-latex-format-headline-function' for details."
          (concat
           ;; Tue Jan 19 16:00:58 EST 2016 - kmodi
           ;; My only change to the original function was to add \\color{red}
           (and todo (format "{\\color{red}\\bfseries\\sffamily %s} " todo))
           (and priority (format "\\framebox{\\#%c} " priority))
           text
           (and tags
                (format "\\hfill{}\\textsc{%s}"
                        (mapconcat (lambda (tag) (org-latex-plain-text tag info))
                                   tags ":")))))))

;;;;; ox-html - HTML export
    (use-package ox-html
      :config
      (progn
        ;; https://www.mathjax.org/cdn-shutting-down/
        (setq org-html-mathjax-options
              (delq (assoc 'path org-html-mathjax-options) org-html-mathjax-options))
        (add-to-list 'org-html-mathjax-options
                     '(path "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-AMS_HTML"))

        ;; ox-html patches
        (load (expand-file-name
               "ox-html-patches.el"
               (concat user-emacs-directory "elisp/patches/"))
              nil :nomessage)

        ;; Remove HTML tags from in-between <title>..</title> else they show
        ;; up verbatim in the browser tabs e.g. "Text <br> More Text"
        (defun modi/ox-html-remove-tags-from-title-tag (orig-return-val)
          (replace-regexp-in-string ".*<title>.*\\(<.*>\\).*</title>.*"
                                    ""
                                    orig-return-val
                                    :fixedcase :literal 1))
        (advice-add 'org-html--build-meta-info :filter-return
                    #'modi/ox-html-remove-tags-from-title-tag)

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
                :class "table-striped"))

        ;; Customize the HTML postamble
        ;; http://thread.gmane.org/gmane.emacs.orgmode/104502/focus=104526
        (defun modi/org-html-postamble-fn (info)
          "My custom postamble for Org to HTML exports.
INFO is the property list of export options.

Date is not included in the postable if date is not set using
\"#+date\" keyword, or if it is set to nil using \"#+date:\" in
the Org document.

Author is not included in the postable if author is set to nil
using \"#+author:\" in the Org document."
          (let* ((author (car (plist-get info :author)))
                 ;; Replace dots, if any, with spaces: "First.Last" -> "First Last"
                 (author (when (stringp author)
                           (replace-regexp-in-string "\\." " " author)))
                 (creator (plist-get info :creator))
                 (date-raw (car (org-export-get-date info)))
                 (date (when date-raw
                         (org-export-data date-raw info)))
                 (d1 "<div style=\"display: inline\" ")
                 (d2 "</div>"))
            (concat "Exported using "
                    d1 "class=\"creator\">" creator d2 ;emacs and Org versions
                    (when author
                      (concat " by " author))
                    (when date
                      (concat " on " d1 "class=\"date\">" date d2))
                    ".")))
        (setq org-html-postamble #'modi/org-html-postamble-fn) ;default: 'auto

        (setq org-html-htmlize-output-type 'css)   ;default: 'inline-css
        (setq org-html-htmlize-font-prefix "org-") ;default: "org-"

        (define-minor-mode modi/org-html-export-on-save-mode
          "Minor mode to enable Org export to HTML when saving the buffer.

Example use: Add below at the end of Org files that you would like to export
on each save.

  * Local Variables :noexport:
  # Local Variables:
  # eval: (modi/org-html-export-on-save-mode 1)
  # End:
"
          :init-value nil
          :lighter "AutoExp"
          (if (and modi/org-html-export-on-save-mode
                   (derived-mode-p 'org-mode))
              (add-hook 'after-save-hook #'org-html-export-to-html nil :local)
            (remove-hook 'after-save-hook #'org-html-export-to-html :local)))

        (use-package ox-html-fancybox
          :load-path "elisp/ox-html-fancybox")))

;;;;; ox-beamer - Beamer export
    (use-package ox-beamer
      :defer t
      :config
      (progn
        ;; Allow for export=>beamer by placing
        ;; #+latex_class: beamer in Org files
        (add-to-list 'org-latex-classes
                     '("beamer"
                       "\\documentclass[presentation]{beamer}"
                       ("\\section{%s}" . "\\section*{%s}")
                       ("\\subsection{%s}" . "\\subsection*{%s}")
                       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))))

;;;;; ox-odt - ODT, doc export
    ;; http://stackoverflow.com/a/22990257/1219634
    (use-package ox-odt
      :disabled
      :config
      (progn
        ;; Auto convert the exported .odt to .doc (MS Word 97) format
        ;; Requires the soffice binary packaged with openoffice
        (setq org-odt-preferred-output-format "doc")))

;;;; External Exporters


;;;;; ox-reveal - Presentations using reveal.js
    (use-package ox-reveal
      ;; Use the local version instead of the one from Melpa, because the
      ;; Melpa version ox-reveal.el has “;; Package-Requires: ((org "20150330"))”
      ;; which installs the Org package from Melpa even though I have a newer
      ;; Org version in `load-path' installed from its git master branch.
      :load-path "elisp/ox-reveal"
      :config
      (progn
        ;; (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
        (setq org-reveal-root "https://cdn.rawgit.com/hakimel/reveal.js/3.4.1/")
        ;; https://www.mathjax.org/cdn-shutting-down/
        (setq org-reveal-mathjax-url "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
        (setq org-reveal-hlevel 1)
        (setq org-reveal-theme "simple") ;beige blood moon night serif simple sky solarized
        (setq org-reveal-mathjax t) ;Use mathjax.org to render LaTeX equations

        ;; Override the `org-reveal-export-to-html' function to generate
        ;; files with “_slides” suffix. So “man.org” will export to
        ;; “man_slides.html”. That way we can have separate html files from
        ;; html and reveal exports.
        (defun org-reveal-export-to-html
            (&optional async subtreep visible-only body-only ext-plist)
          "Export current buffer to a reveal.js HTML file."
          (interactive)
          (let* ((extension (concat "_slides." org-html-extension))
                 (file (org-export-output-file-name extension subtreep))
                 (clientfile (org-export-output-file-name
                              (concat "_client" extension) subtreep)))

            ;; export filename_client HTML file if multiplexing
            (setq client-multiplex nil)
            (setq retfile (org-export-to-file 'reveal file
                            async subtreep visible-only body-only ext-plist))

            ;; export the client HTML file if client-multiplex is set true
            ;; by previous call to org-export-to-file
            (if (eq client-multiplex t)
                (org-export-to-file 'reveal clientfile
                  async subtreep visible-only body-only ext-plist))
            (cond (t retfile)))))
      ;; Do not print date in the reveal title slide
      ;;   #+options: date:nil
      ;; Do not print file time stamp in the reveal title slide
      ;;   #+options: timestamp:nil
      )

;;;;; ox-minutes - Meeting Minutes ASCII export
    (use-package ox-minutes
      :load-path "elisp/ox-minutes")

;;;; Helper Packages for Export

;;;;; Convert included pdf to image
    ;; Replace included pdf files with images when saving Org files.
    (use-package org-include-img-from-pdf
      :load-path "elisp/org-include-img-from-pdf"
      :config
      (progn
        ;; ;; Execute `org-include-img-from-pdf' before saving the file.
        ;; (defun modi/org-include-img-from-pdf-before-save ()
        ;;   "Execute `org-include-img-from-pdf' just before saving the file."
        ;;   (add-hook 'before-save-hook #'org-include-img-from-pdf nil :local))
        ;; (add-hook 'org-mode-hook #'modi/org-include-img-from-pdf-before-save)
        ;; Execute `org-include-img-from-pdf' before exporting.
        (with-eval-after-load 'ox
          (add-hook 'org-export-before-processing-hook #'org-include-img-from-pdf))))

;;;;; Extract image from included .zip
    ;; Auto extract images from zip files when saving Org files.
    (use-package org-include-img-from-archive
      :load-path "elisp/org-include-img-from-archive"
      :config
      (progn
        ;; ;; Execute `modi/org-include-img-from-archive' before saving the file.
        ;; (defun modi/org-include-img-from-archive-before-save ()
        ;;   "Execute `modi/org-include-img-from-archive' just before saving the file."
        ;;   (add-hook 'before-save-hook #'modi/org-include-img-from-archive nil :local))
        ;; (add-hook 'org-mode-hook #'modi/org-include-img-from-archive-before-save)
        ;; Execute `modi/org-include-img-from-archive' before exporting.
        (with-eval-after-load 'ox
          (add-hook 'org-export-before-processing-hook #'modi/org-include-img-from-archive))))))

;;; Org Src
(use-package org-src
  :defer t
  :config
  (progn
    ;; Do not add the default indentation of 2 spaces when exiting the *Org Src*
    ;; buffer (the buffer you get when you do «C-c '» while in a block like
    ;; #+begin_src
    (setq org-edit-src-content-indentation 0) ;Default = 2

    (add-to-list 'org-src-lang-modes '("systemverilog" . verilog))
    (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))
    (add-to-list 'org-src-lang-modes '("go-html-template" . html)) ;Go Template
    (add-to-list 'org-src-lang-modes '("go-text-template" . html)) ;Go Template

    (bind-keys
     :map org-src-mode-map
     ("C-c C-c" . org-edit-src-exit))))

;;; Org Capture
(use-package org-capture
  :defer t
  :config
  (progn
    ;; See `org-capture-templates' doc-string for info on Capture templates
    (if (eq modi/org-version-select 'dev)
        (progn
          ;; http://lists.gnu.org/archive/html/emacs-orgmode/2017-02/msg00084.html
          (add-to-list 'org-capture-templates
                       '("j"          ;`org-capture' binding + j
                         "Journal"
                         entry
                         (file+olp+datetree "journal.org")
                         "\n* %?\n  Entered on %U")))
      (add-to-list 'org-capture-templates
                   '("j"              ;`org-capture' binding + j
                     "Journal"
                     entry
                     (file+datetree "journal.org")
                     "\n* %?\n  Entered on %U")))
    (add-to-list 'org-capture-templates
                 '("n"                ;`org-capture' binding + n
                   "Note"
                   entry
                   (file "") ;empty string defaults to `org-default-notes-file'
                   "\n* %?\n  Context:\n    %i\n  Entered on %U"))))

;;; Org Agenda
(use-package org-agenda
  :defer t
  :config
  (progn
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

    (bind-keys
     :map org-agenda-mode-map
     ("x" . sacha/org-agenda-done)
     ("X" . sacha/org-agenda-mark-done-and-add-followup)
     ("N" . sacha/org-agenda-new))))

;;; Org Table
(use-package org-table
  :defer t
  :config
  (progn
    ;; Do NOT try to auto-evaluate entered text as formula when I begin a
    ;; field's content with "=" e.g. |=123=|. More often than not, I use the "="
    ;; to simply format that field text as verbatim. As now the below variable
    ;; is set to nil, formula will not be automatically evaluated when hitting
    ;; TAB.  But you can still using ‘C-c =’ to evaluate it manually when
    ;; needed.
    (setq org-table-formula-evaluate-inline nil) ;Default = t

;;;; Table Recalculation
    ;; Recalculate all Org tables in the buffer when saving.
    ;; http://emacs.stackexchange.com/a/22221/115
    ;; Thu Jul 14 17:06:28 EDT 2016 - kmodi
    ;; Do not enable the buffer-wide recalculation by default because if an org
    ;; buffer has an org-table formula (like "#+tblfm: $1=@#-1"), a *Calc*
    ;; buffer is created when `org-table-recalculate-buffer-tables' is run each
    ;; time.
    (defvar-local modi/org-table-enable-buffer-wide-recalculation nil
      "When non-nil, all the Org tables in the buffer will be recalculated when
saving the file.

This variable is buffer local.")
    ;; Mark `modi/org-table-enable-buffer-wide-recalculation' as a safe local
    ;; variable as long as its value is t or nil. That way you are not prompted
    ;; to add that to `safe-local-variable-values' in custom.el.
    (put 'modi/org-table-enable-buffer-wide-recalculation 'safe-local-variable #'booleanp)

    (defun modi/org-table-recalculate-buffer-tables (&rest args)
      "Wrapper function for `org-table-recalculate-buffer-tables' that runs
that function only if `modi/org-table-enable-buffer-wide-recalculation' is
non-nil.

Also, this function has optional ARGS that is needed for any function that is
added to `org-export-before-processing-hook'. This would be useful if this
function is ever added to that hook."
      (when modi/org-table-enable-buffer-wide-recalculation
        (org-table-recalculate-buffer-tables)))

    (defun modi/org-table-recalculate-before-save ()
      "Recalculate all Org tables in the buffer before saving."
      (add-hook 'before-save-hook #'modi/org-table-recalculate-buffer-tables nil :local))
    (add-hook 'org-mode-hook #'modi/org-table-recalculate-before-save)

;;;; Table Field Marking
    (defun org-table-mark-field ()
      "Mark the current table field."
      (interactive)
      ;; Do not try to jump to the beginning of field if the point is already there
      (when (not (looking-back "|[[:blank:]]?"))
        (org-table-beginning-of-field 1))
      (set-mark-command nil)
      (org-table-end-of-field 1))

    (defhydra hydra-org-table-mark-field
      (:body-pre (org-table-mark-field)
       :color red
       :hint nil)
      "
   ^^      ^🠙^     ^^
   ^^      _p_     ^^
🠘 _b_  selection  _f_ 🠚          | Org table mark ▯field▮ |
   ^^      _n_     ^^
   ^^      ^🠛^     ^^
"
      ("x" exchange-point-and-mark "exchange point/mark")
      ("f" (lambda (arg)
             (interactive "p")
             (when (eq 1 arg)
               (setq arg 2))
             (org-table-end-of-field arg)))
      ("b" (lambda (arg)
             (interactive "p")
             (when (eq 1 arg)
               (setq arg 2))
             (org-table-beginning-of-field arg)))
      ("n" next-line)
      ("p" previous-line)
      ("q" nil "cancel" :color blue))

    (bind-keys
     :map org-mode-map
     :filter (org-at-table-p)
     ("S-SPC" . hydra-org-table-mark-field/body))))

;;; Org Entities
(use-package org-entities
  :config
  (progn
    ;; http://www.mail-archive.com/emacs-orgmode@gnu.org/msg100527.html
    ;; http://emacs.stackexchange.com/a/16746/115
    (defun modi/org-entity-get-name (char)
      "Return the entity name for CHAR. For example, return \"ast\" for *."
      (let ((ll (append org-entities-user
                        org-entities))
            e name utf8)
        (catch 'break
          (while ll
            (setq e (pop ll))
            (when (not (stringp e))
              (setq utf8 (nth 6 e))
              (when (string= char utf8)
                (setq name (car e))
                (throw 'break name)))))))

    (defun modi/org-insert-org-entity-maybe (&rest args)
      "When the universal prefix C-u is used before entering any character,
insert the character's `org-entity' name if available.

If C-u prefix is not used and if `org-entity' name is not available, the
returned value `entity-name' will be nil."
      ;; It would be fine to use just (this-command-keys) instead of
      ;; (substring (this-command-keys) -1) below in emacs 25+.
      ;; But if the user pressed "C-u *", then
      ;;  - in emacs 24.5, (this-command-keys) would return "^U*", and
      ;;  - in emacs 25.x, (this-command-keys) would return "*".
      ;; But in both versions, (substring (this-command-keys) -1) will return
      ;; "*", which is what we want.
      ;; http://thread.gmane.org/gmane.emacs.orgmode/106974/focus=106996
      (let ((pressed-key (substring (this-command-keys) -1))
            entity-name)
        (when (and (listp args) (eq 4 (car args)))
          (setq entity-name (modi/org-entity-get-name pressed-key))
          (when entity-name
            (setq entity-name (concat "\\" entity-name "{}"))
            (insert entity-name)
            (message (concat "Inserted `org-entity' "
                             (propertize entity-name
                                         'face 'font-lock-function-name-face)
                             " for the symbol "
                             (propertize pressed-key
                                         'face 'font-lock-function-name-face)
                             "."))))
        entity-name))

    ;; Run `org-self-insert-command' only if `modi/org-insert-org-entity-maybe'
    ;; returns nil.
    (advice-add 'org-self-insert-command :before-until #'modi/org-insert-org-entity-maybe)))

;;; Org Babel
(use-package ob
  :defer t
  :init
  (progn
    (defvar modi/ob-enabled-languages `("emacs-lisp"
                                        "org"
                                        "latex"
                                        "dot" ;graphviz
                                        "ditaa"
                                        "plantuml"
                                        "awk"
                                        "python"
                                        ,(if (version< (org-version) "8.3")
                                             "sh" ;ob-shell.el was called ob-sh.el in older Org versions
                                           "shell"))
      "List of languages for which the ob-* packages need to be loaded.")

    ;; Requires installing ob-tcl.el from Org contrib.
    ;; If `ob-tcl' is available add "tcl" to `modi/ob-enabled-languages'.
    (when (require 'ob-tcl nil :noerror)
      (add-to-list 'modi/ob-enabled-languages "tcl"))

    (defvar modi/ob-eval-unsafe-languages '("emacs-lisp"
                                            "shell")
      "List of languages which are unsafe for babel evaluation without
confirmation. Languages present in `modi/ob-enabled-languages' will be marked
as safe for babel evaluation except for the languages in this variable.")

    (let (ob-lang-alist)
      (dolist (lang modi/ob-enabled-languages)
        (add-to-list 'ob-lang-alist `(,(intern lang) . t)))
      (org-babel-do-load-languages 'org-babel-load-languages ob-lang-alist))

    (defun modi/org-confirm-babel-evaluate-fn (lang body)
      "Returns a non-nil value if the user should be prompted for execution,
or nil if no prompt is required.

Babel evaluation will happen without confirmation for the Org src blocks for
the languages in `modi/ob-enabled-languages'."
      (let ((re-all-lang (regexp-opt modi/ob-enabled-languages 'words))
            (re-unsafe-lang (regexp-opt modi/ob-eval-unsafe-languages 'words))
            (unsafe t)) ;Set the return value `unsafe' to t by default
        (when (and (not (string-match-p re-unsafe-lang lang))
                   (string-match-p re-all-lang lang))
          (setq unsafe nil))
        ;; (message "re-all:%s\nre-unsafe:%s\nlang:%s\nbody:%S\nret-val:%S"
        ;; re-all-lang re-unsafe-lang lang body unsafe)
        unsafe))
    (setq org-confirm-babel-evaluate #'modi/org-confirm-babel-evaluate-fn)))

;;; Org Babel Tangle
(use-package ob-tangle
  :defer t
  :config
  (progn
    ;; Trailing whitespace management
    ;; Delete trailing whitespace in tangled buffer and save it.
    (add-hook 'org-babel-post-tangle-hook #'delete-trailing-whitespace)
    (add-hook 'org-babel-post-tangle-hook #'save-buffer :append)))

;;;; Diagrams
(use-package ob-ditaa
  :defer t
  :config
  (progn
    ;; http://pages.sachachua.com/.emacs.d/Sacha.html
    (setq org-ditaa-jar-path (expand-file-name
                              "ditaa.jar"
                              (concat user-emacs-directory "software/")))))

(use-package ob-plantuml
  :defer t
  :config
  (progn
    (setq org-plantuml-jar-path (expand-file-name
                                 "plantuml.jar"
                                 (concat user-emacs-directory "software/")))

    (defun modi/advice-org-babel-execute:plantuml (orig-fun &rest args)
      "Force `shell-file-name' to be bash as the \">\" operator is used for redirection.

If this forcing is not done, and if `shell-file-name' is tcsh,
\">\" does not work.  When trying to overwrite files, we get a
\"File exists\" error, and \">!\" would need to be used instead.

Instead it's simpler to use bash."
      (let ((shell-file-name (executable-find "bash")))
        (apply orig-fun args)))
    (advice-add 'org-babel-execute:plantuml :around #'modi/advice-org-babel-execute:plantuml)))

;;;; Python
(use-package ob-python
  :defer t
  :config
  (progn
    (setq org-babel-python-command "python3"))) ;Default to python 3.x

;;; Other Org Packages

;;;; Org Tree Slide
;; https://github.com/takaxp/org-tree-slide
(use-package org-tree-slide
  :bind (:map modi-mode-map
         ("<C-S-f8>" . org-tree-slide-mode))
  :config
  (progn
    (setq org-tree-slide--lighter " Slide")

    (defvar modi/org-tree-slide-text-scale 3
      "Text scale ratio to default when `org-tree-slide-mode' is enabled.")

    (defun modi/org-tree-slide-set-profile ()
      "Customize org-tree-slide variables."
      (interactive)
      (setq modi/org-tree-slide-text-scale 3)
      (setq org-tree-slide-header nil)
      (setq org-tree-slide-slide-in-effect t)
      (setq org-tree-slide-slide-in-blank-lines 5)
      (setq org-tree-slide-heading-emphasis nil)
      (setq org-tree-slide-cursor-init t) ;Move cursor to the head of buffer
      (setq org-tree-slide-modeline-display 'lighter)
      (setq org-tree-slide-skip-done nil)
      (setq org-tree-slide-skip-comments t)
      (setq org-tree-slide-activate-message
            (concat "Starting Org presentation. "
                    "Use left/right arrow keys to navigate the slides."))
      (setq org-tree-slide-deactivate-message "Ended presentation.")
      (message "my custom `org-tree-slide' profile: ON"))

    (defun modi/org-tree-slide-start ()
      "Set up the frame for the slideshow."
      (interactive)
      (when (bound-and-true-p writegood-mode)
        (puthash "writegood-mode" writegood-mode modi/states)
        (writegood-mode -1))
      (when (bound-and-true-p beacon-mode)
        (puthash "beacon-mode" beacon-mode modi/states)
        (beacon-mode -1))
      (when (bound-and-true-p hl-line-when-idle-p)
        (puthash "hl-line-idle" hl-line-when-idle-p modi/states)
        (toggle-hl-line-when-idle))
      (let ((is-fullscreen (frame-parameter nil 'fullscreen)))
        (unless is-fullscreen
          (puthash "disable-fullscreen" t modi/states)
          (toggle-frame-fullscreen)))
      (modi/toggle-one-window :force-one-window) ;force 1 window
      (setq org-hide-emphasis-markers t)
      (modi/org-tree-slide-set-profile)
      (text-scale-set modi/org-tree-slide-text-scale)
      (setq cursor-type 'bar))
    (add-hook 'org-tree-slide-play-hook #'modi/org-tree-slide-start)

    (defun modi/org-tree-slide-stop()
      "Undo the frame setup for the slideshow."
      (interactive)
      (modi/toggle-one-window) ;toggle 1 window
      (when (gethash "writegood-mode" modi/states)
        (writegood-mode 1)
        (remhash "writegood-mode" modi/states))
      (when (gethash "beacon-mode" modi/states)
        (beacon-mode 1)
        (remhash "beacon-mode" modi/states))
      (when (gethash "hl-line-idle" modi/states)
        (toggle-hl-line-when-idle)
        (remhash "hl-line-idle" modi/states))
      (when (gethash "disable-fullscreen" modi/states)
        (toggle-frame-fullscreen)
        (remhash "disable-fullscreen" modi/states))
      (setq org-hide-emphasis-markers nil)
      (text-scale-set 0)
      (setq cursor-type t))
    (add-hook 'org-tree-slide-stop-hook #'modi/org-tree-slide-stop)

    (defun modi/org-tree-slide-text-scale-reset ()
      "Reset time scale to `modi/org-tree-slide-text-scale'."
      (interactive)
      (text-scale-set modi/org-tree-slide-text-scale))

    (defun modi/org-tree-slide-text-scale-inc1 ()
      "Increase text scale by 1."
      (interactive)
      (text-scale-increase 1))

    (defun modi/org-tree-slide-text-scale-dec1 ()
      "Decrease text scale by 1."
      (interactive)
      (text-scale-decrease 1))

    (bind-keys
     :map org-tree-slide-mode-map
     ("<left>" . org-tree-slide-move-previous-tree)
     ("<right>" . org-tree-slide-move-next-tree)
     ("C-0" . modi/org-tree-slide-text-scale-reset)
     ("C-=" . modi/org-tree-slide-text-scale-inc1)
     ("C--" . modi/org-tree-slide-text-scale-dec1)
     ("C-1" . org-tree-slide-content)
     ("C-2" . modi/org-tree-slide-set-profile)
     ("C-3" . org-tree-slide-simple-profile)
     ("C-4" . org-tree-slide-presentation-profile))))

;;;; Org Cliplink
;; https://github.com/rexim/org-cliplink
(use-package org-cliplink
  :bind (:map org-mode-map
         ;; "C-c C-l" is bound to `org-insert-link' by default.
         ;; "C-c C-L" is bound to `org-cliplink'.
         ("C-c C-S-l" . org-cliplink)))

;;;; Sticky Header Line
;; https://github.com/alphapapa/org-sticky-header
(use-package org-sticky-header
  :ensure t
  :config
  (progn
    (setq org-sticky-header-full-path 'full) ;'reversed, nil
    ;; Always show the header if the option to show the full or reversed
    ;; path is set.
    (setq org-sticky-header-always-show-header (if org-sticky-header-full-path t nil))

    ;; https://github.com/alphapapa/org-sticky-header/pull/20
    (defun org-sticky-header--fetch-stickyline ()
      "Return string of Org heading or outline path for display in header line."
      (org-with-wide-buffer
       (goto-char (window-start))
       (if (org-before-first-heading-p)
           ""
         (progn
           ;; No non-header lines above top displayed header
           (when (or org-sticky-header-always-show-header
                     (not (org-at-heading-p)))
             ;; Header should be shown
             (when (fboundp 'org-inlinetask-in-task-p)
               ;; Skip inline tasks
               (while (and (org-back-to-heading)
                           (org-inlinetask-in-task-p))
                 (forward-line -1)))
             (cond
              ;; FIXME: Convert cond back to pcase, but one compatible with Emacs 24
              ((null org-sticky-header-full-path)
               (concat (org-sticky-header--get-prefix)
                       (org-get-heading t t)))
              ((eq org-sticky-header-full-path 'full)
               (concat (org-sticky-header--get-prefix)
                       (org-format-outline-path (org-get-outline-path t)
                                                (window-width)
                                                nil org-sticky-header-outline-path-separator)))
              ((eq org-sticky-header-full-path 'reversed)
               (let ((s (concat (org-sticky-header--get-prefix)
                                (mapconcat 'identity
                                           (nreverse (org-split-string (org-format-outline-path (org-get-outline-path t)
                                                                                                1000 nil "")
                                                                       ""))
                                           org-sticky-header-outline-path-reversed-separator))))
                 (if (> (length s) (window-width))
                     (concat (substring s 0 (- (window-width) 2))
                             "..")
                   s)))
              (t "")))))))
    (add-hook 'org-mode-hook #'org-sticky-header-mode)))

;;;; Org Link Ref
;; Support markdown-style link id references
(use-package org-link-ref
  :load-path "elisp/org-link-ref")

;;;; Htmlize Region→File
(use-package htmlize-r2f
  :load-path "elisp/htmlize-r2f"
  :bind (:map region-bindings-mode-map
         ("H" . htmlize-r2f))
  :bind (:map modi-mode-map
         ("C-c H" . htmlize-r2f)))

;;;; Include Src lines
;; Auto-update line numbers for source code includes when saving Org files.
(use-package org-include-src-lines
  :load-path "elisp/org-include-src-lines"
  :config
  (progn
    ;; Execute `endless/org-include-update' before saving the file.
    (defun modi/org-include-update-before-save ()
      "Execute `endless/org-include-update' just before saving the file."
      (add-hook 'before-save-hook #'endless/org-include-update nil :local))
    (add-hook 'org-mode-hook #'modi/org-include-update-before-save)))

;;;; Org TOC
;; https://github.com/snosov1/toc-org
;; Used in https://github.com/kaushalmodi/ox-hugo/blob/master/doc/export-gh-doc.el
(use-package toc-org
  :ensure t)

;;; Provide
(provide 'setup-org)

;;; Notes
;; C-c C-e l l <-- Do Org > tex
;; C-c C-e l p <-- Do Org > tex > pdf using the command specified in
;;                 `org-latex-pdf-process' list
;; C-c C-e l o <-- Do Org > tex > pdf and open the pdf file using the app
;;                 associated with pdf files defined in `org-file-apps'

;; When the cursor is on an existing link, `C-c C-l' allows you to edit the link
;; and description parts of the link.

;; C-c C-x f <-- Insert footnote

;; C-c C-x C-l <-- Preview latex fragment in place; C-c C-c to exit that preview.

;; Auto-completions https://orgmode.org/manual/Completion.html
;; \ M-TAB <- TeX symbols
;; ​* M-TAB <- Headlines; useful when doing [[* Partial heading M-TAB when linking to headings
;; #+ M-TAB <- org-mode special keywords like #+date, #+author, etc

;; Speed-keys are awesome! https://orgmode.org/manual/Speed-keys.html

;; Disable selected org-mode markup character on per-file basis
;; http://emacs.stackexchange.com/a/13231/115

;; Installing minted.sty
;; In order to have that tex convert to pdf, you have to ensure that you have
;; minted.sty in your TEXMF folder.
;;  - To know if minted.sty in correct path do "kpsewhich minted.sty".
;;  - If it is not found, download from http://www.ctan.org/tex-archive/macros/latex/contrib/minted
;;  - Generate minted.sty by "tex minted.ins"
;;  - To know your TEXMF folder, do "kpsewhich -var-value=TEXMFHOME"
;;  - For me TEXMF folder was ~/texmf
;;  - Move the minted.sty to your $TEXMF/tex/latex/commonstuff folder.
;;  - Do mkdir -p ~/texmf/tex/latex/commonstuff if that folder hierarchy doesn't exist
;;  - Do "mktexlsr" to refresh the sty database
;;  - Generate pdf from the Org exported tex by "pdflatex -shell-escape FILE.tex"

;; Sources for Org > tex > pdf conversion:
;;  - http://nakkaya.com/2010/09/07/writing-papers-using-org-mode/
;;  - http://mirrors.ctan.org/macros/latex/contrib/minted/minted.pdf

;; To have an Org document auto update the #+date: keyword during exports, use:
;;   #+date: {{{time(%b %d %Y\, %a)}}}
;; The time format here can be anything as documented in `format-time-string' fn.

;; Controlling section numbering, levels in `ox-twbs' exports:
;; https://github.com/marsmining/ox-twbs/issues/10#issuecomment-140324367
;;
;;   #+options: num:5 whn:2 toc:4 H:6
;;
;; Above would mean,
;; - Create section numbers up to level 5 (num).
;; - Display section numbers up to level 2 (whn).
;; - Display table of contents 4 deep (toc).
;; - Consider sections after 6 to be "low-level" (H).

;; How to mark the whole src block that the point is in?
;; C-c C-v C-M-h (`org-babel-mark-block')

;; How to toggle display of inline images?
;; C-c C-x C-v (`org-toggle-inline-images')

;; Setting buffer local variables to be effective during Org exports
;; http://thread.gmane.org/gmane.emacs.orgmode/107058
;; If you want to set a buffer local variable foo to nil during Org exports,
;; add the below to the end of the Org file
;;
;;   #+bind: foo nil
;;   # L**ocal Variables:
;;   # org-export-allow-bind-keywords: t
;;   # E**nd:
;;
;; Above ** are added in the local variables footer so that that is not
;; recognized as the local variables setup for THIS file.

;; C-u M-RET - Force create a heading (see `org-insert-heading') whether point
;; is in a list or a normal line.

;; Local Variables:
;; aggressive-indent-mode: nil
;; End:
