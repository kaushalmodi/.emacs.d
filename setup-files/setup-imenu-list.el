;; Time-stamp: <2015-07-06 09:09:17 kmodi>

;; Imenu-list
;; https://github.com/bmag/imenu-list

(use-package imenu-list
  :config
  (progn
    (setq imenu-list-size     0.2)
    (setq imenu-list-position 'right)

    (defun modi/imenu-list-hide ()
      (interactive)
      (switch-to-buffer-other-window imenu-list-buffer-name)
      (quit-window))

    (defun modi/imenu-list-display-toggle (noselect)
      "Toggle the display of Imenu-list buffer.

If NOSELECT is non-nil, do not select the imenu-list buffer."
      (interactive "P")
      (let ((imenu-list-visible))
        (dolist (win (window-list))
          (when (string-match-p "\\`\\*Ilist\\*\\'" (buffer-name (window-buffer win)))
            (setq imenu-list-visible t)))
        (if imenu-list-visible
            (modi/imenu-list-hide)
          (if noselect
              (imenu-list-noselect)
            (imenu-list)))))

    (defun modi/imenu-list-goto-entry-and-hide ()
      "Execute `imenu-list-goto-entry' and hide the imenu-list buffer."
      (interactive)
      (imenu-list-goto-entry)
      (modi/imenu-list-hide))
    (bind-key "C-<return>"
              #'modi/imenu-list-goto-entry-and-hide
              imenu-list-major-mode-map)))


(provide 'setup-imenu-list)
