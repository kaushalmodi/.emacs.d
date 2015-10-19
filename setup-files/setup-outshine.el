;; Time-stamp: <2015-09-16 10:36:51 kmodi>

;; Outshine
;; https://github.com/tj64/outshine

;; (defvar outline-minor-mode-prefix "\M-#")
;; Above needs to be set using Customize so that it is set BEFORE the
;; `outline' library is loaded (not `outshine').

(use-package outshine
  :load-path "elisp/outshine"
  :config
  (progn
    (setq outshine-use-speed-commands t)
    (setq outshine-org-style-global-cycling-at-bob-p t)

    ;; http://emacs.stackexchange.com/a/2803/115
    (defun modi/outline-toc ()
      "Create a table of contents for outshine headers.

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

Don't add “Revision Control” heading to TOC."
      (interactive)
      (save-excursion
        (goto-char (point-min))
        (let ((outline-comment-start
               (concat "\\(\\s<"
                       (when comment-start
                         (concat
                          "\\|"
                          ;; trim white space from comment-start
                          (replace-regexp-in-string " " "" comment-start)))
                       "\\)"))
              (el-mode (derived-mode-p 'emacs-lisp-mode))
              parsed-outline-comment-start
              headings-list stars-list
              heading star)
          ;; (message "%s" outline-comment-start)
          (while (re-search-forward
                  (concat "^\\(?1:" ; beginning of line
                          outline-comment-start
                          (if el-mode
                              (concat "\\{2\\}\\)" ; 2 consecutive ; in `emacs-lisp-mode'
                                      ";\\(?2:;*\\)") ; followed by one or more ; chars
                            (concat "\\s-\\{1\\}\\)" ; SINGLE white space
                                    "\\*\\(?2:\\**\\)")) ; followed by one or more * chars
                          " " ; followed by a space
                          "\\(?3:.+\\)") ; followed by heading
                  nil :noerror)
            (setq parsed-outline-comment-start (match-string-no-properties 1))
            ;; Note that the below `star' var stores one less * than the actual;
            ;; that's intentional. Also note that for `emacs-lisp-mode' the 3rd
            ;; consecutive ; onwards is counted as a “star”.
            (setq star    (match-string-no-properties 2))
            (setq heading (match-string-no-properties 3))
            ;; (message "%s %s %s" parsed-outline-comment-start star heading)
            (when (not (string= heading "Revision Control"))
              (setq stars-list    (cons star stars-list))
              (setq headings-list (cons heading headings-list))))
          (setq stars-list    (nreverse stars-list))
          (setq headings-list (nreverse headings-list))

          (goto-char (point-min))
          (while (re-search-forward
                  (concat "^"
                          outline-comment-start
                          (when el-mode
                            "\\{2\\}") ; 2 consecutive ; in `emacs-lisp-mode'
                          " Contents:")
                  nil :noerror)
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
                       ";; " ; 2 consecutive ; in `emacs-lisp-mode'
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
                  (setq n (1+ n)))))))))

    (defvar modi/outline-mode-hooks '(verilog-mode-hook
                                      emacs-lisp-mode-hook
                                      conf-space-mode-hook) ; for .tmux.conf
      "List of hooks of major modes in which outline-mode should be enabled.")

    (defun modi/turn-on-outline-mode ()
      "Turn on outline-mode only for specific modes."
      (interactive)
      (dolist (hook modi/outline-mode-hooks)
        (add-hook hook #'outline-minor-mode)
        (add-hook hook (lambda () (add-hook 'before-save-hook #'modi/outline-toc nil :local)))))

    (defun modi/turn-off-outline-mode ()
      "Turn off outline-mode only for specific modes."
      (interactive)
      (dolist (hook modi/outline-mode-hooks)
        (remove-hook hook #'outline-minor-mode)
        (remove-hook hook (lambda () (remove-hook 'before-save-hook #'modi/outline-toc :local)))))

    (modi/turn-on-outline-mode)

    ;; Hook `outshine' to `outline-mode'
    (add-hook 'outline-minor-mode-hook #'outshine-hook-function)

    (with-eval-after-load 'outline
      (use-package foldout
        :config
        (progn
          (bind-keys
           :map outline-minor-mode-map
            ("C-c C-z" . foldout-zoom-subtree)
            ("C-c C-x" . foldout-exit-fold)))))

    ;; Mirror the default org-mode behavior in `outline-minor-mode-map'
    (bind-keys
     :map outline-minor-mode-map
      ("<backtab>" . outshine-cycle-buffer) ; global cycle using S-TAB
      ("M-p"       . outline-previous-visible-heading)
      ("M-n"       . outline-next-visible-heading)
      ("<M-up>"    . outline-move-subtree-up)
      ("<M-down>"  . outline-move-subtree-down)
      ("<M-left>"  . outline-promote)
      ("<M-right>" . outline-demote))

    (key-chord-define outline-minor-mode-map "JJ" #'outshine-imenu)))


(provide 'setup-outshine)

;; To Debug:
;; When outshine is enabled, it remaps self-insert-command to
;; outshine-self-insert-command. That works fine except that in
;; emacs-lisp-mode when outline-mode is enabled (and thus outshine is
;; enabled), the eldoc-mode is messed up.
;; Example: after typing `(define-key' followed by SPACE, the eldoc-mode
;; should show the hint for `define-key' in the echo area. But that
;; does not happen while outshine is enabled. It starts working fine
;; if I disable outline-mode (and thus outshine too).
