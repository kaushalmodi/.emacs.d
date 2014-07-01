;; Time-stamp: <2014-07-01 14:16:19 kmodi>

;; Shell Script Mode

(defun my-sh-mode-customizations()
  (when (boundp 'setup-linum-loaded)
    (nlinum-mode 1))
  )
(add-hook 'sh-mode-hook 'my-sh-mode-customizations)


(provide 'setup-shell)
