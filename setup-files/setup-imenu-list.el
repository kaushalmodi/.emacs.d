;; Time-stamp: <2016-05-19 22:27:27 kmodi>

;; Imenu-list
;; https://github.com/bmag/imenu-list

(use-package imenu-list
  :commands (modi/imenu-list-display-toggle)
  :config
  (progn
    (setq imenu-list-size     0.2)
    (setq imenu-list-position 'right)

    (defun modi/imenu-list-hide ()
      (interactive)
      (switch-to-buffer-other-window imenu-list-buffer-name)
      (quit-window))

    (defun modi/imenu-list-visible-p ()
      "Returns `t' if the `imenu-list' buffer is visible."
      (catch 'break
        (dolist (win (window-list))
          (when (string= imenu-list-buffer-name (buffer-name (window-buffer win)))
            (throw 'break t)))))

    (defun modi/imenu-list-display-toggle (noselect)
      "Toggle the display of Imenu-list buffer.

If NOSELECT is non-nil, do not select the imenu-list buffer."
      (interactive "P")
      (if (modi/imenu-list-visible-p)
          (modi/imenu-list-hide)
        (if noselect
            (imenu-list-noselect)
          (imenu-list))))

    (defun modi/imenu-list-goto-entry-and-hide ()
      "Execute `imenu-list-goto-entry' and hide the imenu-list buffer."
      (interactive)
      (imenu-list-goto-entry)
      (modi/imenu-list-hide))
    (bind-key "C-<return>"
              #'modi/imenu-list-goto-entry-and-hide
              imenu-list-major-mode-map)

    (defun modi/imenu-auto-update (orig-fun &rest args)
      "Auto update the *Ilist* buffer if visible."
      (prog1 ; Return value of the advising fn needs to be the same as ORIG-FUN
          (apply orig-fun args)
        (when (modi/imenu-list-visible-p)
          (imenu-list-update-safe)))) ; update `imenu-list' buffer
    (advice-add 'switch-to-buffer :around #'modi/imenu-auto-update)
    (advice-add 'revert-buffer    :around #'modi/imenu-auto-update)))


(provide 'setup-imenu-list)
