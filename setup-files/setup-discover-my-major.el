;; Time-stamp: <2015-02-23 11:42:33 kmodi>

;; Discover-My-Major
;; Source: https://github.com/steckerhalter/discover-my-major

(use-package discover-my-major
  :init
  (progn
    (bind-keys
     :map modi-mode-map
     ("C-h C-m" . discover-my-major))))


(provide 'setup-discover-my-major)
