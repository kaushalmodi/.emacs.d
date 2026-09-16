;; -*- lexical-binding: t; -*-
;; Time-stamp: <2020-09-10 23:08:37 kmodi>

;; Shell Script Mode

(use-package sh-script
  ;; `sh-mode' is remapped to `bash-ts-mode' in setup-treesitter.el. The
  ;; `shell-script-mode' alias is not, so the entries below that can be
  ;; csh/tcsh or other non-bash shells keep using the classic `sh-mode'.
  :mode (("\\.sh\\'"          . sh-mode)
         ("\\.alias\\'"       . shell-script-mode)
         ("\\.gpms\\'"        . shell-script-mode)
         ("\\.cfg\\'"         . shell-script-mode)
         ("\\.t?csh\\'"       . shell-script-mode)
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
    (advice-add 'sh-make-vars-local :override #'modi/sh-make-vars-local)))

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
