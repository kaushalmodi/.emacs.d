;; Time-stamp: <2015-03-31 10:20:35 kmodi>

;; Keyfreq
;; https://github.com/dacap/keyfreq

(use-package keyfreq
  :if (not (bound-and-true-p disable-pkg-keyfreq))
  :config
  (progn
    (setq keyfreq-file      (expand-file-name ".keyfreq"      user-emacs-directory))
    (setq keyfreq-file-lock (expand-file-name ".keyfreq.lock" user-emacs-directory))
    (keyfreq-mode 1)
    (keyfreq-autosave-mode 1)

    (defun my/keyfreq-save-html ()
      "Save the table of frequently used commands (and their associated bindings
to an html file in `user-emacs-directory'."
      (interactive)
      (keyfreq-html (expand-file-name "keyfreq.html" user-emacs-directory)))))


(provide 'setup-keyfreq)
