;; Time-stamp: <2014-08-13 10:38:39 kmodi>

;; Expand Region
;; Source: https://github.com/magnars/expand-region.el

(req-package expand-region
  :init
  (progn
    (setq expand-region-contract-fast-key "|"
          expand-region-reset-fast-key    "<ESC><ESC>")

    (defun my-expand-region ()
      "Be default `er/expand-region' takes the cursor to the beginning of the
selected text. I'd like the cursor to end up at the end of that text."
      (interactive)
      (er/expand-region 1)
      (exchange-point-and-mark))

    (bind-keys
     :map modi-mode-map
     ;; bind er/expand-region to `C-\' instead of `C-=' because `=' sign clashes
     ;; when trying to wrap a selection with `=' in org-mode using the wrap-region
     ;; package
     ("C-\\" . my-expand-region))))


(provide 'setup-expand-region)
