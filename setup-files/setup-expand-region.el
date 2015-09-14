;; Time-stamp: <2015-09-13 23:55:44 kmodi>

;; Expand Region
;; https://github.com/magnars/expand-region.el

(use-package expand-region
  :commands (er/expand-region)
  :init
  (progn
    (bind-keys
     :map modi-mode-map
      ;; bind er/expand-region to `C-\' instead of `C-=' because `=' sign
      ;; clashes when trying to wrap a selection with `=' in org-mode using the
      ;; `wrap-region' package
      ("C-\\" . er/expand-region)))
  :config
  (progn
    (setq expand-region-contract-fast-key "|")
    (setq expand-region-reset-fast-key    "<ESC><ESC>")))


(provide 'setup-expand-region)
