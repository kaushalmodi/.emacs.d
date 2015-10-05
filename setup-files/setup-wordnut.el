;; Time-stamp: <2015-10-05 10:32:14 kmodi>

;; Wordnut
;; https://github.com/gromnitsky/wordnut

;; This package requires the user to have installed the `wn' application
;; from https://wordnet.princeton.edu/wordnet/download/current-version/
;; It is tested to work with Wordnet 3.0.

(use-package wordnut
  :bind (:map modi-mode-map
         ("C-c / /" . wordnut-search))
  :config
  (progn
    (bind-keys
     :map wordnut-mode-map
      ("q" . modi/quit-and-kill-window))))


(provide 'setup-wordnut)
