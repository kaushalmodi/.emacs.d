;; Time-stamp: <2015-03-09 13:51:01 kmodi>

;; Fold This
;; https://github.com/magnars/fold-this.el

(use-package fold-this
    :config
  (progn
    (setq fold-this-persistent-folds t)

    ;; Below bindings are made in global map and not in my minor mode as I want
    ;; other modes to override those bindings
    (bind-keys
     ("C-c C-f" . fold-this))))


(provide 'setup-fold-this)
