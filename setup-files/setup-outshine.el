;; Time-stamp: <2019-01-02 10:54:47 kmodi>

;; Outshine
;; https://github.com/tj64/outshine

(use-package outline
  ;; It is NECESSARY that the `outline-minor-mode-prefix' variable is set to
  ;; "\M-#" BEFORE `outline' library is loaded. After loading the library,
  ;; changing this prefix key requires manipulating keymaps.
  :preface
  (setq outline-minor-mode-prefix "\M-#")
  :defer t
  :init
  (progn
    (defvar modi/outline-minor-mode-hooks '(verilog-mode-hook
                                            emacs-lisp-mode-hook
                                            conf-space-mode-hook) ;For .tmux.conf
      "List of hooks of major modes in which `outline-minor-mode' should be enabled.")

    (defun modi/turn-on-outline-minor-mode ()
      "Turn on `outline-minor-mode' only for specific modes."
      (interactive)
      (dolist (hook modi/outline-minor-mode-hooks)
        (add-hook hook #'outline-minor-mode)))

    (defun modi/turn-off-outline-minor-mode ()
      "Turn off `outline-minor-mode' only for specific modes."
      (interactive)
      (dolist (hook modi/outline-minor-mode-hooks)
        (remove-hook hook #'outline-minor-mode)))

    (modi/turn-on-outline-minor-mode))
  :config
  (progn
    ;; Always enable Outshine in `outline-minor-mode'
    (add-hook 'outline-minor-mode-hook #'outshine-mode)

    (defun modi/outline-next-visible-heading (arg)
      "Move to the next visible heading line.
    With ARG, repeats or can move backward if negative.
    A heading line is one that starts with a `*' (or that
    `outline-regexp' matches)."
      (interactive "p")
      (if (< arg 0)
          (beginning-of-line)
        (end-of-line))
      (let (found-heading-p)
        (while (and (not (bobp)) (< arg 0))
          (while (and (not (bobp))
                      (setq found-heading-p
                            (re-search-backward
                             (concat "^"
                                     (if (bound-and-true-p modi/outshine-allow-space-before-heading)
                                         "[[:blank:]]*"
                                       "")
                                     "\\(?:" outline-regexp "\\)")
                             nil 'move))
                      (outline-invisible-p)))
          (setq arg (1+ arg)))
        (while (and (not (eobp)) (> arg 0))
          (while (and (not (eobp))
                      (setq found-heading-p
                            (re-search-forward
                             (concat "^"
                                     (if (bound-and-true-p modi/outshine-allow-space-before-heading)
                                         "[[:blank:]]*"
                                       "")
                                     "\\(?:" outline-regexp "\\)")
                             nil 'move))
                      (outline-invisible-p (match-beginning 0))))
          (setq arg (1- arg)))
        (if found-heading-p (beginning-of-line))))
    (advice-add 'outline-next-visible-heading :override #'modi/outline-next-visible-heading)

    ;; http://emacs.stackexchange.com/a/2803/115
    (defun modi/outline-toc ()
      "Create a table of contents for outline/outshine headers.

For `emacs-lisp-mode':
 - The Contents header has to be “;; Contents:”
 - Level 1 headers will be of the form “;;; L1 Header”
 - Level 2 headers will be of the form “;;;; L2 Header”
 - ..

For other major modes:
 - The Contents header has to be “<comment-start> Contents:”
 - Level 1 headers will be of the form “<comment-start> * L1 Header”
 - Level 2 headers will be of the form “<comment-start> ** L2 Header”
 - ..

The function will end silently if the Contents header is not found in the file.

Don't add “Revision Control” heading to TOC."
      (interactive)
      (save-excursion
        (let* ((outline-comment-start
                (concat "\\(\\s<"
                        (when comment-start
                          (concat
                           "\\|"
                           ;; trim white space from comment-start
                           ;; `regexp-quote' is used to escape characters like `*'
                           ;; when `comment-start' holds a value like "/*".
                           (replace-regexp-in-string " " "" (regexp-quote comment-start))))
                        "\\)"))
               (el-mode (derived-mode-p 'emacs-lisp-mode))
               (content-header-regexp (concat "^"
                                              outline-comment-start
                                              (when el-mode
                                                "\\{2\\}") ;2 consecutive ;in `emacs-lisp-mode'
                                              " Contents:")))
          (goto-char (point-min))
          (when (re-search-forward content-header-regexp nil :noerror)
            ;; Continue with parsing the whole file only if a "Contents:"
            ;; comment header exists in the file.
            (let ((contents-start-point (point))
                  parsed-outline-comment-start
                  headings-list stars-list
                  heading star)
              ;; (message "%s" outline-comment-start)
              (while (re-search-forward
                      (concat "^"       ;beginning of line
                              (if (bound-and-true-p modi/outshine-allow-space-before-heading)
                                  "[[:blank:]]*"
                                "")
                              "\\(?1:"
                              outline-comment-start
                              (if el-mode
                                  (concat "\\{2\\}\\)" ;2 consecutive ;in `emacs-lisp-mode'
                                          ";\\(?2:;*\\)") ;followed by one or more ;chars
                                (concat "\\s-\\)"        ;SINGLE white space
                                        "\\*\\(?2:\\**\\)")) ;followed by one or more * chars
                              " "                        ;followed by a space
                              "\\(?3:.+\\)")               ;followed by heading
                      nil :noerror)
                (setq parsed-outline-comment-start (match-string-no-properties 1))
                ;; Note that the below `star' var stores one less * than the actual;
                ;; that's intentional. Also note that for `emacs-lisp-mode' the 3rd
                ;; consecutive ;onwards is counted as a “star”.
                (setq star    (match-string-no-properties 2))
                (setq heading (match-string-no-properties 3))
                ;; (message "%s %s %s" parsed-outline-comment-start star heading)
                (when (not (string= heading "Revision Control"))
                  (setq stars-list    (cons star stars-list))
                  (setq headings-list (cons heading headings-list))))
              (setq stars-list    (nreverse stars-list))
              (setq headings-list (nreverse headings-list))

              (goto-char contents-start-point)
              (forward-line 1)
              ;; First delete old contents
              ;; Keep on going on to the next line till it reaches a blank line
              (while (progn
                       (when (looking-at (concat "^" outline-comment-start))
                         ;; Delete current line without saving to kill-ring
                         (let (p1 p2)
                           (save-excursion
                             (setq p1 (line-beginning-position))
                             (next-line 1)
                             (setq p2 (line-beginning-position))
                             (delete-region p1 p2))))
                       (not (looking-at "^\n"))))
              ;; Then print table of contents
              (let ((content-comment-prefix
                     (if el-mode
                         ";; " ;2 consecutive ;in `emacs-lisp-mode'
                       parsed-outline-comment-start)))
                (insert (format "%s\n" content-comment-prefix))
                (let ((n 1))
                  (dolist (h headings-list)
                    ;; (insert (format "// %2d. %s\n" n heading))
                    (insert (format "%s %s%s\n"
                                    content-comment-prefix
                                    (replace-regexp-in-string
                                     (if el-mode ";" "\\*") "  " (pop stars-list))
                                    h))
                    (setq n (1+ n))))))))))

    ;; Call `modi/outline-toc' on file saves *only* in major modes where
    ;; `outline-minor-mode' is enabled.
    (defun modi/outline-add-update-toc-hook ()
      "Auto-generate/update TOC on file saves."
      (add-hook 'before-save-hook #'modi/outline-toc nil :local))
    (add-hook 'outline-minor-mode-hook #'modi/outline-add-update-toc-hook)

    (use-package foldout
      :config
      (progn
        (bind-keys
         :map outline-minor-mode-map
         ("C-c C-z" . foldout-zoom-subtree)
         ("C-c C-x" . foldout-exit-fold))))

    (defun modi/outline-minor-mode-bindings ()
      "Mirror few default Org bindings in `outline-minor-mode-map'."
      (bind-keys
       :map outline-minor-mode-map
       ("M-p" . outline-previous-visible-heading)
       ("M-n" . outline-next-visible-heading)
       ("<M-up>" . outline-move-subtree-up)
       ("<M-down>" . outline-move-subtree-down)
       ("<M-left>" . outline-promote)
       ("<M-right>" . outline-demote)))
    (modi/outline-minor-mode-bindings)))

(use-package outshine
  :defer t
  :config
  (progn
    (setq outshine-use-speed-commands t)
    (setq outshine-org-style-global-cycling-at-bob-p t)

    (defvar-local modi/outshine-allow-space-before-heading nil
      "When non-nil, allow outshine heading to begin with whitespace.
For example, when non-nil, do not require the \"// *\" style
comments used by `outshine' to start at column 0 in `verilog-mode.'")

    (defun modi/outshine-calc-outline-regexp (orig-ret)
      "Prefix the outline regexp with whitespace regexp, may be.
Do this if `modi/outshine-allow-space-before-heading' is non-nil."
      (let ((ret orig-ret))
        (when modi/outshine-allow-space-before-heading
          (setq ret (concat "[[:blank:]]*" orig-ret)))
        ret))
    (advice-add 'outshine-calc-outline-regexp :filter-return #'modi/outshine-calc-outline-regexp)
    ;; (advice-remove 'outshine-calc-outline-regexp #'modi/outshine-calc-outline-regexp)

    ;; Thu Aug 24 18:37:49 EDT 2017 - kmodi
    ;; Why is the `outline-minor-mode-hook' called twice? Who is calling it the second time?!
    ;; Answer: https://debbugs.gnu.org/cgi/bugreport.cgi?bug=28229#8

    (bind-keys
     :map outline-minor-mode-map
     ("<backtab>" . outshine-cycle-buffer)) ;Global cycle using S-TAB
    ;; Override the `outline-minor-mode-map' bindings set by `outshine'.
    (modi/outline-minor-mode-bindings)))


(provide 'setup-outshine)
