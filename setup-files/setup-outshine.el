;; Time-stamp: <2015-05-04 14:46:37 kmodi>

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
    (defun modi/outline-table-of-contents ()
      "Create a table of content for outshine headers.
Insert/update the TOC after the line that has the “// Contents:” string.
Here “//” represents 2 comment start characters for any major mode.
Don't add “Revision Control” heading to TOC."
      (interactive)
      (save-excursion
        (goto-char (point-min))
        (let ((outline-comment-start (concat "^" ; beginning of line
                                             "\\(\\(\\s<\\|"
                                             (substring comment-start 0 1)
                                             "\\)\\{2\\}\\)")) ; 2 comment chars
              parsed-outline-comment-start
              headings-list stars-list
              heading star)
          ;; (message "%s" outline-comment-start)
          (while (re-search-forward
                  (concat outline-comment-start " " ; followed by space
                          "\\*\\(\\**\\) " ; one or more * chars followed by space
                          "\\(.+\\)") ; followed by heading
                  nil :noerror)
            (setq parsed-outline-comment-start (match-string 1))
            (setq star                         (match-string 3))
            (setq heading                      (match-string 4))
            ;; (message "%s %s %s" parsed-outline-comment-start star heading)
            (when (not (string= heading "Revision Control"))
              (setq stars-list    (cons star stars-list))
              (setq headings-list (cons heading headings-list))))
          (setq stars-list    (nreverse stars-list))
          (setq headings-list (nreverse headings-list))

          (goto-char (point-min))
          (while (re-search-forward
                  (concat outline-comment-start " " ; followed by space
                          "Contents:")
                  nil :noerror)
            (forward-line 1)
            ;; First delete old contents
            ;; Keep on going on to the next line till it reaches a blank line
            (while (progn
                     (when (looking-at outline-comment-start)
                       ;; Delete current line without saving to kill-ring
                       (let (p1 p2)
                         (save-excursion
                           (setq p1 (line-beginning-position))
                           (next-line 1)
                           (setq p2 (line-beginning-position)))
                         (delete-region p1 p2)))
                     (not (looking-at "^\n"))))
            ;; Then print table of contents
            (insert (format "%s\n" parsed-outline-comment-start))
            (let ((n 1))
              (dolist (h headings-list)
                ;; (insert (format "// %2d. %s\n" n heading))
                (insert (format "%s %s%s\n"
                                parsed-outline-comment-start
                                (replace-regexp-in-string
                                 "\\*" "  " (pop stars-list))
                                h))
                (setq n (1+ n))))))))

    (defvar modi/outline-mode-hooks '(verilog-mode-hook
                                      emacs-lisp-mode-hook)
      "List of hooks of major modes in which outline-mode should be enabled.")

    (defun modi/turn-on-outline-mode ()
      "Turn on outline-mode only for specific modes."
      (interactive)
      (dolist (hook modi/outline-mode-hooks)
        (add-hook hook #'outline-minor-mode)
        (add-hook hook (λ (add-hook 'local-write-file-hooks
                                    #'modi/outline-table-of-contents)))))

    (defun modi/turn-off-outline-mode ()
      "Turn off outline-mode only for specific modes."
      (interactive)
      (dolist (hook modi/outline-mode-hooks)
        (remove-hook hook #'outline-minor-mode)
        (remove-hook hook (λ (remove-hook 'local-write-file-hooks
                                          #'modi/outline-table-of-contents)))))

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

    (bind-keys
     :map outline-minor-mode-map
      ("<M-up>"   . nil)
      ("M-p"      . outline-previous-visible-heading)
      ("<M-down>" . nil)
      ("M-n"      . outline-next-visible-heading))

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
