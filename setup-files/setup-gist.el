;; Time-stamp: <2016-11-19 23:54:37 kmodi>

;; Gist
;; https://github.com/defunkt/gist.el

(use-package gist
  :defer t
  :config
  (progn
    ;; https://github.com/defunkt/gist.el/issues/82#issuecomment-261757035
    (setq gist-list-format
          '((created "Created" 15 nil "%D %R")
            (id "Id" 10 nil identity)
            (files "Files" 35 nil (lambda (files)
                                    (mapconcat 'identity files ", ")))
            (visibility "Private" 8 nil (lambda (public)
                                              (or (and public "") "   Y")))
            (description "Description" 0 nil identity)))))


(provide 'setup-gist)
