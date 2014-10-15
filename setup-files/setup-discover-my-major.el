;; Time-stamp: <2014-10-14 11:53:17 kmodi>

;; Discover-My-Major
;; Source: https://github.com/steckerhalter/discover-my-major

(req-package discover-my-major
  :init
  (progn
    (bind-keys
     :map modi-mode-map
     ("C-h C-m" . discover-my-major))))


(provide 'setup-discover-my-major)
