;; Time-stamp: <2015-01-28 09:28:36 kmodi>

;; Pcache

(req-package pcache
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
