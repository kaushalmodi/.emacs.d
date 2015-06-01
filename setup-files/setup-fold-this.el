;; Time-stamp: <2015-06-01 14:31:28 kmodi>

;; Fold This
;; https://github.com/magnars/fold-this.el

(use-package fold-this
  :init
  (progn
    (setq fold-this-persistent-folds-file (locate-user-emacs-file
                                           "fold-this-cache.el")))
  :config
  (progn
    (setq fold-this-persistent-folds t)
    ;; Below bindings are made in global map and not in my minor mode as I want
    ;; other modes to override those bindings
    (bind-keys
     ("C-c C-f" . fold-this))))


(provide 'setup-fold-this)
