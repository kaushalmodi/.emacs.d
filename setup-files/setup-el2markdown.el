;; Time-stamp: <2016-08-22 18:42:59 kmodi>

;; https://github.com/Lindydancer/el2markdown

(use-package el2markdown
  :bind (:map modi-mode-map
         ("C-c R" . el2markdown-write-readme)))


(provide 'setup-el2markdown)
