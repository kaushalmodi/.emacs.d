;; Time-stamp: <2015-12-10 14:58:51 kmodi>

;; Pcache

(use-package pcache
  :preface
  (progn
    (setq pcache-directory (let ((dir (concat user-emacs-directory
                                              "var_" emacs-version-short "/pcache/")))
                             (make-directory dir :parents)
                             dir))))


(provide 'setup-pcache)
