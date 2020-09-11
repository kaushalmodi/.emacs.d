;; Time-stamp: <2020-09-10 23:08:37 kmodi>

;; Shell Script Mode

(use-package sh-script
  :mode (("\\.alias\\'"       . shell-script-mode)
         ("\\.gpms\\'"        . shell-script-mode)
         ("\\.cfg\\'"         . shell-script-mode)
         ("\\.c*sh\\'"        . shell-script-mode)
         ("\\.[a-zA-Z]+rc\\'" . shell-script-mode)
         ("crontab.*\\'"     . shell-script-mode))
  :config
  (progn
    ;; https://github.com/Tux/tcsh/blob/master/csh-mode.el
    ;; For `csh-indent-line' and `csh-indent-region'
    (use-package csh-mode
      :load-path "elisp/csh-mode")

    ;; Change default shell file to bash if available
    (when-let* ((bash-bin (executable-find "bash")))
      (setq-default sh-shell-file bash-bin))

    ;; Thu Mar 30 15:41:39 EDT 2017 - kmodi
    ;; Below function is the same as original except that that message is not
    ;; displayed at the end.
    (defun modi/sh-make-vars-local ()
      "Make the indentation variables local to this buffer.
Normally they already are local.  This command is provided in case
variable `sh-make-vars-local' has been set to nil.

To revert all these variables to the global values, use
command `sh-reset-indent-vars-to-global-values'."
      (interactive)
      (mapc 'make-local-variable sh-var-list)
      ;; (message "Indentation variables are now local.")
      )
    (advice-add 'sh-make-vars-local :override #'modi/sh-make-vars-local)

    ;; Mon Feb 10 08:31:05 EST 2020 - kmodi
    ;; Most (probably all?) of the below code got deprecated in https://git.savannah.gnu.org/cgit/emacs.git/commit/?id=f9504ffba2e2604338c243dd77c877bbb8162e4a.
    (>=e "28.0"
        nil
      (progn
        (setq sh-indent-supported '((sh . sh)
                                    (rc . rc)
                                    (csh . csh)
                                    (tcsh . csh)))

        (defun modi/sh-set-shell (shell &optional no-query-flag insert-flag)
          "Set this buffer's shell to SHELL (a string).
When used interactively, insert the proper starting #!-line,
and make the visited file executable via `executable-set-magic',
perhaps querying depending on the value of `executable-query'.

When this function is called noninteractively, INSERT-FLAG (the third
argument) controls whether to insert a #!-line and think about making
the visited file executable, and NO-QUERY-FLAG (the second argument)
controls whether to query about making the visited file executable.

Calls the value of `sh-set-shell-hook' if set.

Shell script files can cause this function be called automatically
when the file is visited by having a `sh-shell' file-local variable
whose value is the shell name (don't quote it)."
          (interactive (list (completing-read
                              (format "Shell (default %s): " sh-shell-file)
                              ;; This used to use interpreter-mode-alist, but that is
                              ;; no longer appropriate now that uses regexps.
                              ;; Maybe there could be a separate variable that lists
                              ;; the shells, used here and to construct i-mode-alist.
                              ;; But the following is probably good enough:
                              (append (mapcar (lambda (e) (symbol-name (car e)))
                                              sh-ancestor-alist)
                                      '("csh" "rc" "sh"))
                              nil nil nil nil sh-shell-file)
                             (eq executable-query 'function)
                             t))
          (if (string-match "\\.exe\\'" shell)
              (setq shell (substring shell 0 (match-beginning 0))))
          ;; (message "shell: %s" shell)

          (setq sh-shell (sh-canonicalize-shell shell))
          ;; (message "sh-shell: %s" sh-shell)

          (if insert-flag
              (setq sh-shell-file
                    (executable-set-magic shell (sh-feature sh-shell-arg)
                                          no-query-flag insert-flag)))
          (setq mode-line-process (format "[%s]" sh-shell))
          (setq-local sh-shell-variables nil)
          (setq-local sh-shell-variables-initialized nil)
          (setq-local imenu-generic-expression
                      (sh-feature sh-imenu-generic-expression))
          (let ((tem (sh-feature sh-mode-syntax-table-input)))
            (when tem
              (setq-local sh-mode-syntax-table (apply 'sh-mode-syntax-table tem))
              (set-syntax-table sh-mode-syntax-table)))
          (dolist (var (sh-feature sh-variables))
            (sh-remember-variable var))
          (setq-local sh-indent-supported-here (sh-feature sh-indent-supported))
          (cond
           ((or (eq sh-shell 'sh)
                (eq sh-shell 'bash)
                (eq sh-shell 'rc))
            ;; (message "sh-indent-supported-here: %s" sh-indent-supported-here)
            ;; (message "Setting up indent for shell type %s" sh-shell)
            (let ((mksym (lambda (name)
                           (intern (format "sh-smie-%s-%s"
                                           sh-indent-supported-here name)))))
              (add-function :around (local 'smie--hanging-eolp-function)
                (lambda (orig)
                  (if (looking-at "[ \t]*\\\\\n")
                      (goto-char (match-end 0))
                    (funcall orig))))
              (add-hook 'smie-indent-functions #'sh-smie--indent-continuation nil t)
              (smie-setup (symbol-value (funcall mksym "grammar"))
                          (funcall mksym "rules")
                          :forward-token  (funcall mksym "forward-token")
                          :backward-token (funcall mksym "backward-token")))
            (setq-local parse-sexp-lookup-properties t)
            (unless sh-use-smie
              (setq-local sh-kw-alist (sh-feature sh-kw))
              (let ((regexp (sh-feature sh-kws-for-done)))
                (if regexp
                    (setq-local sh-regexp-for-done
                                (sh-mkword-regexpr (regexp-opt regexp t)))))
              ;; (message "setting up indent stuff")
              ;; sh-mode has already made indent-line-function local
              ;; but do it in case this is called before that.
              (setq-local indent-line-function #'sh-indent-line))
            (when sh-make-vars-local
              (sh-make-vars-local))
            ;; (message "Indentation setup for shell type %s" sh-shell)
            )
           ((or (eq sh-shell 'tcsh)
                (eq sh-shell 'csh))
            (setq-local indent-line-function #'csh-indent-line)
            (setq-local indent-region-function #'csh-indent-region))
           ((null sh-indent-supported-here)
            (message "No indentation for this shell type.")
            (setq-local indent-line-function #'sh-basic-indent-line)))
          (when font-lock-mode
            (setq font-lock-set-defaults nil)
            (font-lock-set-defaults)
            (font-lock-flush))
          (setq sh-shell-process nil)
          (run-hooks 'sh-set-shell-hook))
        (advice-add 'sh-set-shell :override #'modi/sh-set-shell)))
    ))

(defun modi/shell-region (start end)
  "Execute region in a shell corresponding to the local value of `sh-shell'.

After the execution, the output buffer is displayed, the point is moved to it,
and the output buffer mode is set to the read-only `special-mode'."
  (interactive "r")
  (let ((shell-file-name (executable-find (symbol-name sh-shell)))
        (output-buf "*Shell Region Output*"))
    (message "Executing the region in `%s' shell .."
             (file-name-nondirectory shell-file-name))
    (shell-command (buffer-substring-no-properties start end) output-buf)
    (pop-to-buffer output-buf)
    ;; Set the major mode to `special-mode' so that you can quit window with q.
    (special-mode)))
;; Bind C-x C-e to `modi/shell-region' ONLY if the current major mode is
;; `sh-mode' and if a region is selected.
(bind-keys
 :map modi-mode-map
 :filter (and (derived-mode-p 'sh-mode)
              (use-region-p))
 ("C-x C-e" . modi/shell-region))


(provide 'setup-shell)
