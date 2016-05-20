;; Time-stamp: <2016-05-19 23:20:21 kmodi>

;; Abbrev
(use-package abbrev
  :config
  (progn
    (setq abbrev-file-name (locate-user-emacs-file "abbrev_defs"))
    (unless (file-exists-p abbrev-file-name)
      (with-temp-buffer (write-file abbrev-file-name)))
    (setq save-abbrevs 'silently) ; Silently save abbrevs on quitting emacs

    (defconst modi/abbrev-hooks '(verilog-mode-hook
                                  emacs-lisp-mode-hook
                                  org-mode-hook)
      "List of hooks of major modes in which abbrev should be enabled.")

    (defun modi/turn-on-abbrev-mode ()
      "Turn on abbrev only for specific modes."
      (interactive)
      (dolist (hook modi/abbrev-hooks)
        (when (eq hook 'verilog-mode-hook)
          (with-eval-after-load 'verilog-mode
            ;; Reset the verilog-mode abbrev table
            (clear-abbrev-table verilog-mode-abbrev-table)))
        (add-hook hook #'abbrev-mode)))

    (defun modi/turn-off-abbrev-mode ()
      "Turn off abbrev only for specific modes."
      (interactive)
      (dolist (hook modi/abbrev-hooks)
        (remove-hook hook #'abbrev-mode)))

    (modi/turn-on-abbrev-mode)
    (quietly-read-abbrev-file))) ; Reads the abbreviations file on startup

;; Hippie Expand
(use-package hippie-exp
  :bind (:map modi-mode-map
         ("M-/" . hippie-expand)))


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
