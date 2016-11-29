;; Time-stamp: <2016-11-28 23:10:06 kmodi>

;; Hungry Delete
;; https://github.com/nflath/hungry-delete

(use-package hungry-delete
  :config
  (progn
    (setq hungry-delete-chars-to-skip " \t\r\f\v")

    ;; Mon Nov 21 08:45:42 EST 2016 - kmodi
    ;; Override the default definitions of `hungry-delete-skip-ws-forward' and
    ;; `hungry-delete-skip-ws-backward'; do not delete back-slashes at EOL.
    (defun hungry-delete-skip-ws-forward ()
      "Skip over any whitespace following point.
This function skips over horizontal and vertical whitespace."
      (skip-chars-forward hungry-delete-chars-to-skip)
      (while (get-text-property (point) 'read-only)
        (backward-char)))

    (defun hungry-delete-skip-ws-backward ()
      "Skip over any whitespace preceding point.
This function skips over horizontal and vertical whitespace."
      (skip-chars-backward hungry-delete-chars-to-skip)
      (while (get-text-property (point) 'read-only)
        (forward-char)))

    (defun modi/turn-off-hungry-delete-mode ()
      "Turn off hungry delete mode."
      (hungry-delete-mode -1))

    ;; Enable `hungry-delete-mode' everywhere ..
    (global-hungry-delete-mode)

    ;; Except ..
    ;; `hungry-delete-mode'-loaded backspace does not work in `wdired-mode',
    ;; i.e. when editing file names in the *Dired* buffer.
    (add-hook 'wdired-mode-hook #'modi/turn-off-hungry-delete-mode)
    ;; and in minibuffer
    (add-hook 'minibuffer-setup-hook #'modi/turn-off-hungry-delete-mode)))


(provide 'setup-hungry-delete)
