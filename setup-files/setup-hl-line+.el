;; Time-stamp: <2014-08-12 17:41:44 kmodi>

;; hl-line+
;; Source: http://www.emacswiki.org/emacs/hl-line%2B.el

(req-package hl-line+
  :config
  (progn
    (toggle-hl-line-when-idle 1) ; Highlight line only when idle
    (setq hl-line-flash-show-period 2) ;; Number of seconds for `hl-line-flash' to highlight the line
    (bind-keys
     :map modi-mode-map
     ("C-c C-f" . hl-line-flash))))


(provide 'setup-hl-line+)
