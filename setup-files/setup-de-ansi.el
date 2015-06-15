;; Time-stamp: <2015-06-15 17:08:48 kmodi>

(use-package de-ansi
  :load-path "elisp/de-ansi"
  :commands (de-ansify)
  :init
  (progn
    (bind-to-modi-map "d" #'de-ansify)))


(provide 'setup-de-ansi)
