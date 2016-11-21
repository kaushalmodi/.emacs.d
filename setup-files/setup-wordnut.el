;; Time-stamp: <2016-11-21 07:47:39 kmodi>

;; Wordnut
;; https://github.com/gromnitsky/wordnut

;; This package requires the user to have installed the `wn' application
;; from https://wordnet.princeton.edu/wordnet/download/current-version/
;; It is tested to work with Wordnet 3.0.

(use-package wordnut
  :bind (:map modi-mode-map
         ;; I don't want to override the `org-sparse-tree' binding (C-c /) in
         ;; org-mode.
         ("C-x / /" . wordnut-search))
  :config
  (progn
    (bind-keys
     :map wordnut-mode-map
      ("q" . modi/quit-and-kill-window))))


(provide 'setup-wordnut)
