;; Time-stamp: <2015-06-10 17:30:04 kmodi>

;; Writegood
;; https://github.com/bnbeckwith/writegood-mode

(use-package writegood-mode
  :config
  (progn
    (add-hook 'org-mode-hook #'writegood-mode)))


(provide 'setup-writegood)
