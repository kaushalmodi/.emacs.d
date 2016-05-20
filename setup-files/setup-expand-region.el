;; Time-stamp: <2016-05-19 22:06:14 kmodi>

;; Expand Region
;; https://github.com/magnars/expand-region.el

(use-package expand-region
  :bind (:map modi-mode-map
         ;; Bind `er/expand-region' to C-\ instead of C-= because the = sign
         ;; clashes when trying to wrap a selection with = in org-mode using
         ;; the `wrap-region' package.
         ("C-\\" . er/expand-region))
  :config
  (progn
    (setq expand-region-contract-fast-key "|")
    (setq expand-region-reset-fast-key "<ESC><ESC>")))


(provide 'setup-expand-region)
