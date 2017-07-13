;; Time-stamp: <2017-07-12 17:02:44 kmodi>

;; Hugo
;; https://gohugo.io

(use-package ox-hugo
  :after ox
  :load-path "elisp/ox-hugo"
  :bind (:map modi-mode-map
         ("C-c G" . org-hugo-publish-subtree)))


(provide 'setup-hugo)
