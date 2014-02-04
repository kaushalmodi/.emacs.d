;; Time-stamp: <2013-12-02 17:06:00 kmodi>

;; Stripe Mode
;; Source: https://github.com/sabof/stripe-buffer/blob/master/README.md

(require 'cl-lib)
(require 'stripe-buffer)

(add-hook 'dired-mode-hook 'stripe-listify-buffer) ;; stripify dired
(add-hook 'org-mode-hook 'turn-on-stripe-table-mode) ;; stripify tables in org-mode

(provide 'setup-stripe-buffer)
