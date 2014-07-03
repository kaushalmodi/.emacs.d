;; Time-stamp: <2014-07-03 11:26:09 kmodi>

;; Stripe Mode
;; Source: https://github.com/sabof/stripe-buffer/blob/master/README.md

(require 'cl-lib)
(require 'stripe-buffer)

(add-hook 'org-mode-hook 'turn-on-stripe-table-mode) ;; stripify tables in org-mode

(provide 'setup-stripe-buffer)
