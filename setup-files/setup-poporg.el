;; Time-stamp: <2015-06-02 15:56:23 kmodi>

;; poporg
;; https://github.com/QBobWatson/poporg

;; poporg is a small Emacs Lisp project to help editing program strings and
;; comments using Org mode (or any other major mode).  This can be useful as it
;; is often more convenient to edit large pieces of text, like Emacs Lisp or
;; Python docstrings, in an org-mode buffer instead of in a comment or a string.

(use-package poporg
  :load-path "elisp/poporg"
  :commands (poporg-dwim)
  :init
  (progn
    (bind-to-modi-map "o" poporg-dwim))
  :config
  (progn
    (add-to-list 'display-buffer-alist
                 '("\\*poporg.*?\\*" . ((display-buffer-reuse-window
                                         display-buffer-pop-up-window)
                                        . ((inhibit-same-window . t)))))))


(provide 'setup-poporg)
