;; Time-stamp: <2015-02-24 23:38:41 kmodi>

;; Stripe Mode
;; https://github.com/sabof/stripe-buffer

(use-package stripe-buffer
  :config
  (progn
    (add-hook 'package-menu-mode-hook #'stripe-listify-buffer)
    ;; (add-hook 'dired-mode-hook        #'stripe-listify-buffer)
    (add-hook 'org-mode-hook          #'turn-on-stripe-table-mode)))


(provide 'setup-stripe-buffer)
