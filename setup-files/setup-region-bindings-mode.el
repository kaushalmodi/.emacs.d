;; Time-stamp: <2014-08-12 23:52:13 kmodi>

;; Region Bindings Mode
;; https://github.com/fgallina/region-bindings-mode
;; Minor mode that enables the ability of having a custom keys for working with
;; regions. This is a pretty good way to keep the global bindings clean.

(req-package region-bindings-mode
  :config
  (progn
    (region-bindings-mode-enable)))


(provide 'setup-region-bindings-mode)
