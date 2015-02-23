;; Time-stamp: <2015-02-23 11:42:33 kmodi>

;; Pcache

(use-package pcache
  :config
  (progn
    (setq pcache-directory
          (let ((dir (concat user-emacs-directory
                             "/var_"
                             emacs-version-short
                             "/pcache/")))
            (make-directory dir t)
            dir))))


(provide 'setup-pcache)
