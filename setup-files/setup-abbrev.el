;; Time-stamp: <2015-04-30 11:03:20 kmodi>

;; Abbrev
(use-package abbrev
  :config
  (progn

    (defconst modi/abbrev-hooks '(verilog-mode-hook
                                  emacs-lisp-mode-hook
                                  org-mode-hook)
      "List of hooks of major modes in which abbrev should be enabled.")

    (defun modi/turn-on-abbrev-mode ()
      "Turn on abbrev only for specific modes."
      (interactive)
      (dolist (hook modi/abbrev-hooks)
        (add-hook hook #'abbrev-mode)))

    (defun modi/turn-off-abbrev-mode ()
      "Turn off abbrev only for specific modes."
      (interactive)
      (dolist (hook modi/abbrev-hooks)
        (remove-hook hook #'abbrev-mode)))

    (setq save-abbrevs 'silently) ; silently save abbrevs on quitting emacs

    (modi/turn-on-abbrev-mode)
    (quietly-read-abbrev-file))) ; reads the abbreviations file on startup

;; Hippie Expand
(use-package hippie-exp
  :config
  (progn
    (bind-key "M-/" #'hippie-expand modi-mode-map)))


(provide 'setup-abbrev)

;; By default `C-x a' is bound to `abbrev-map'
;;
;; * Few `abbrev-map' bindings
;; |---------+---------------------------|
;; | Binding | Description               |
;; |---------+---------------------------|
;; | - / ig  | inverse-add-global-abbrev |
;; | il      | inverse-add-local-abbrev  |
;; | g       | add-global-abbrev         |
;; | + / l   | add-mode-abbrev           |
;; | ' / e   | expand-abbrev             |
;; |---------+---------------------------|

;; http://www.star.bris.ac.uk/bjm/emacs-tips.html#sec-1-17
;; Usage example: Now you can type the abbreviation you want, followed by
;; `C-x a -' and you will be prompted for the expanded text.
;;   e.g. fn C-x a - function
