;; Time-stamp: <2015-05-22 12:06:42 kmodi>

;; Pcache

(use-package pcache
  :config
  (progn
    (setq pcache-directory
          (let ((dir (concat user-emacs-directory
                             "var_"
                             emacs-version-short
                             "/pcache/")))
            (make-directory dir t)
            dir))))


(provide 'setup-pcache)
