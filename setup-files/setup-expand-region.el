;; Time-stamp: <2014-06-20 09:29:08 kmodi>

;; Popwin
;; Source: https://github.com/magnars/expand-region.el

(require 'expand-region)

(setq expand-region-contract-fast-key "|"
      expand-region-reset-fast-key    "<ESC>"
      )

;; Key bindings for expand-region are saved in setup-key-bindings.el
;; Search for `setup-expand-region-loaded' there.


(setq setup-expand-region-loaded t)
(provide 'setup-expand-region)
