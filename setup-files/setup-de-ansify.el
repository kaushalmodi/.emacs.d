;; Time-stamp: <2016-02-18 00:33:51 kmodi>

(use-package de-ansify
  :load-path "elisp/de-ansify"
  :commands (de-ansify)
  :init
  (progn
    (bind-to-modi-map "d" #'de-ansify)))


(provide 'setup-de-ansify)
