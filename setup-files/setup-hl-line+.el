;; Time-stamp: <2014-10-28 16:43:02 kmodi>

;; hl-line+
;; Source: http://www.emacswiki.org/emacs/hl-line%2B.el

(req-package hl-line+
  :config
  (progn
    (toggle-hl-line-when-idle 1) ; Highlight line only when idle
    (setq hl-line-flash-show-period 2) ;; Number of seconds for `hl-line-flash' to highlight the line

    ;; Below bindings are made in global map and not in my minor mode as I want
    ;; other modes to override those bindings
    (bind-keys
     ("C-c C-f" . hl-line-flash))))


(provide 'setup-hl-line+)
