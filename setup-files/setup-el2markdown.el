;; Time-stamp: <2016-08-10 11:07:52 kmodi>

;; https://github.com/Lindydancer/el2markdown

(use-package el2markdown
  :load-path "elisp/el2markdown"
  :bind (:map modi-mode-map
         ("C-c R" . el2markdown-write-readme)))


(provide 'setup-el2markdown)
