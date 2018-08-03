;; Time-stamp: <2018-08-03 12:57:08 kmodi>

;; Webpaste
;; https://github.com/etu/webpaste.el

(use-package webpaste
  :defer t
  :init
  (progn
    (bind-to-modi-map "w" #'webpaste-paste-region)
    (with-eval-after-load 'setup-editing
      (advice-add 'webpaste-paste-region :around #'modi/advice-region-or-whole))))


(provide 'setup-webpaste)
