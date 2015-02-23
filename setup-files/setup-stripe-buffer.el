;; Time-stamp: <2015-02-23 11:28:52 kmodi>

;; Stripe Mode
;; https://github.com/sabof/stripe-buffer

(use-package stripe-buffer
  :config
  (progn
    (add-hook 'package-menu-mode-hook #'stripe-listify-buffer)       ; stripify package list
    (add-hook 'dired-mode-hook        #'stripe-listify-buffer)       ; stripify dired
    (add-hook 'org-mode-hook          #'turn-on-stripe-table-mode))) ; stripify tables in org-mode


(provide 'setup-stripe-buffer)
