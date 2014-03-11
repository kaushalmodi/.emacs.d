;; Time-stamp: <2014-03-11 16:57:13 kmodi>

;; Region Bindings Mode
;; https://github.com/fgallina/region-bindings-mode
;; Minor mode that enables the ability of having a custom keys for working with
;; regions. This is a pretty good way to keep the global bindings clean.

(require 'region-bindings-mode)

(region-bindings-mode-enable)

;; Region mode bindings
(when (boundp 'setup-multiple-cursors-loaded)
  (define-key region-bindings-mode-map "a" 'mc/mark-all-like-this)
  (define-key region-bindings-mode-map "p" 'mc/mark-previous-like-this)
  (define-key region-bindings-mode-map "n" 'mc/mark-next-like-this)
  (define-key region-bindings-mode-map "m" 'mc/mark-more-like-this-extended)
  )

(when (boundp 'setup-search-loaded)
  (define-key region-bindings-mode-map "]" 'anzu-query-replace) ;; normal replace in region
  )

(when (boundp 'setup-visual-regexp-loaded)
  (define-key region-bindings-mode-map "}" 'vr/query-replace) ;; regex replace in region
  )


(setq setup-region-bindings-mode-loaded t)
(provide 'setup-region-bindings-mode)
