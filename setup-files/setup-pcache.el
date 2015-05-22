;; Time-stamp: <2015-05-22 13:01:06 kmodi>

;; Pcache

(use-package pcache
  :init
  (progn
    (setq pcache-directory (let ((dir (concat user-emacs-directory
                                              "var_" emacs-version-short "/pcache/")))
                             (make-directory dir :parents)
                             dir))))


(provide 'setup-pcache)
