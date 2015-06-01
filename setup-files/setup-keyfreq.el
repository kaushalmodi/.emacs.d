;; Time-stamp: <2015-06-01 14:21:17 kmodi>

;; Keyfreq
;; https://github.com/dacap/keyfreq

(use-package keyfreq
  :if (not (bound-and-true-p disable-pkg-keyfreq))
  :config
  (progn
    (setq keyfreq-file      (locate-user-emacs-file "keyfreq"))
    (setq keyfreq-file-lock (locate-user-emacs-file "keyfreq.lock"))
    (keyfreq-mode 1)
    (keyfreq-autosave-mode 1)

    (defun my/keyfreq-save-html ()
      "Save the table of frequently used commands (and their associated bindings
to an html file in `user-emacs-directory'."
      (interactive)
      (keyfreq-html (locate-user-emacs-file "keyfreq.html")))))


(provide 'setup-keyfreq)
