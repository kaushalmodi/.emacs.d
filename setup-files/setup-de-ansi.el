;; Time-stamp: <2015-02-24 09:05:21 kmodi>

(use-package de-ansi
  :load-path "elisp/de-ansi"
  :commands (de-ansify)
  :init
  (progn
    (bind-to-modi-map "d" de-ansify)))


(provide 'setup-de-ansi)
