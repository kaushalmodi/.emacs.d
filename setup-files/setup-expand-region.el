;; Time-stamp: <2014-07-15 14:02:30 kmodi>

;; Popwin
;; Source: https://github.com/magnars/expand-region.el

(require 'expand-region)

(setq expand-region-contract-fast-key "|"
      expand-region-reset-fast-key    "<ESC>"
      )

(defun my-expand-region ()
  "Be default `er/expand-region' takes the cursor to the beginning of the
selected text. I'd like the cursor to end up at the end of that text."
  (interactive)
  (er/expand-region 1)
  (exchange-point-and-mark))

;; Key bindings for expand-region are saved in setup-key-bindings.el
;; Search for `setup-expand-region-loaded' there.


(setq setup-expand-region-loaded t)
(provide 'setup-expand-region)
