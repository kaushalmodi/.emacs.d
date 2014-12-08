;; Time-stamp: <2014-12-08 09:14:58 kmodi>

;; Expand Region
;; Source: https://github.com/magnars/expand-region.el

(req-package expand-region
  :init
  (progn
    (setq expand-region-contract-fast-key "|"
          expand-region-reset-fast-key    "<ESC><ESC>")

    (bind-keys
     :map modi-mode-map
     ;; bind er/expand-region to `C-\' instead of `C-=' because `=' sign clashes
     ;; when trying to wrap a selection with `=' in org-mode using the wrap-region
     ;; package
     ("C-\\" . er/expand-region))))


(provide 'setup-expand-region)
