;; Time-stamp: <2014-08-13 11:27:54 kmodi>

;; Stripe Mode
;; Source: https://github.com/sabof/stripe-buffer/blob/master/README.md

(req-package stripe-buffer
  :require (cl-lib)
  :config
  (progn
    (add-hook 'package-menu-mode-hook 'stripe-listify-buffer)     ;; stripify package list
    (add-hook 'dired-mode-hook        'stripe-listify-buffer)     ;; stripify dired
    (add-hook 'org-mode-hook          'turn-on-stripe-table-mode))) ;; stripify tables in org-mode


(provide 'setup-stripe-buffer)
