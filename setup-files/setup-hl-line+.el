;; Time-stamp: <2013-12-04 17:04:03 kmodi>

;; hl-line+
;; Source: http://www.emacswiki.org/emacs/hl-line%2B.el

(require 'hl-line+)

(toggle-hl-line-when-idle 1) ; Highlight line only when idle

(setq hl-line-flash-show-period 2) ;; Number of seconds for `hl-line-flash' to highlight the line

(setq setup-hl-line+-loaded t)
(provide 'setup-hl-line+)
