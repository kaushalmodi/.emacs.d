;; Time-stamp: <2015-02-23 11:42:32 kmodi>

;; poporg
;; poporg is a small Emacs Lisp project to help editing program strings and
;; comments using Org mode (or any other major mode).  This can be useful as it
;; is often more convenient to edit large pieces of text, like Emacs Lisp or
;; Python docstrings, in an org-mode buffer instead of in a comment or a string.

;; README: https://github.com/QBobWatson/poporg

(use-package poporg
  :commands (poporg-dwim)
  :init
  (progn
    (bind-to-modi-map "o" poporg-dwim)))


(provide 'setup-poporg)
