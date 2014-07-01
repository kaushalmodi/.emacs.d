;; Time-stamp: <2014-07-01 14:16:05 kmodi>

;; Emacs Lisp Mode

(defun my-emacs-lisp-mode-customizations()
  (when (boundp 'setup-linum-loaded)
    (nlinum-mode 1))
  )
(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-customizations)


(provide 'setup-elisp)
