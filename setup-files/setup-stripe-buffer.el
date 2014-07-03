;; Time-stamp: <2014-07-03 12:54:42 kmodi>

;; Stripe Mode
;; Source: https://github.com/sabof/stripe-buffer/blob/master/README.md

(require 'cl-lib)
(require 'stripe-buffer)

(add-hook 'package-menu-mode-hook 'stripe-listify-buffer)     ;; stripify package list
(add-hook 'dired-mode-hook        'stripe-listify-buffer)     ;; stripify dired
(add-hook 'org-mode-hook          'turn-on-stripe-table-mode) ;; stripify tables in org-mode


(provide 'setup-stripe-buffer)
