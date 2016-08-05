;; Time-stamp: <2016-08-04 23:10:24 kmodi>

;; Abbrev

(setq save-abbrevs 'silently) ; Silently save abbrevs on quitting emacs

(use-package abbrev
  :config
  (progn
    (setq abbrev-file-name (locate-user-emacs-file "abbrev_defs"))
    (unless (file-exists-p abbrev-file-name)
      (with-temp-buffer (write-file abbrev-file-name)))

    (defconst modi/abbrev-hooks '(prog-mode-hook
                                  org-mode-hook)
      "List of hooks of major modes in which abbrev should be enabled.")

    (defun modi/turn-on-abbrev-mode ()
      "Turn on abbrev only for specific modes."
      (interactive)
      (with-eval-after-load 'verilog-mode
        ;; Reset the verilog-mode abbrev table
        (clear-abbrev-table verilog-mode-abbrev-table))
      (dolist (hook modi/abbrev-hooks)
        (add-hook hook #'abbrev-mode)))

    (defun modi/turn-off-abbrev-mode ()
      "Turn off abbrev only for specific modes."
      (interactive)
      (dolist (hook modi/abbrev-hooks)
        (remove-hook hook #'abbrev-mode)))

    (modi/turn-on-abbrev-mode)
    (quietly-read-abbrev-file))) ; Read the abbreviations file on startup

;; Hippie Expand
(use-package hippie-exp
  :bind (:map modi-mode-map
         ("M-/" . hippie-expand)))


(provide 'setup-abbrev)

;; By default, `C-x a' is bound to `abbrev-map'.
;;
;; - When you have typed BEFORE and you want to add an abbrev to replace that
;;   with AFTER, use the "inverse-add-*-abbrev" commands.
;;   |-----------+---------------------------|
;;   | C-x a i l | inverse-add-mode-abbrev   |
;;   | C-x a i g | inverse-add-global-abbrev |
;;   | C-x a -   | inverse-add-global-abbrev |
;;   |-----------+---------------------------|
;;
;;   Use case:
;;     I have typed "nill" by mistake and I want to always replace "nill" with
;;   "null".
;;
;;     nill C-xail null
;;
;; - When you have already typed AFTER and you want to add an abbrev to have
;;   BEFORE replaced with that AFTER, use the "add-*-abbrev" commands.
;;   |---------+-------------------|
;;   | C-x a l | add-mode-abbrev   |
;;   | C-x a + | add-mode-abbrev   |
;;   | C-x a g | add-global-abbrev |
;;   |---------+-------------------|
;;
;;   Use case:
;;     I have already typed the correct term "null". But if I type "nill" in
;;   future, I want to replace that with "null".
;;
;;     null C-xal nill
;;
;; - `abbrev-mode' *needs* to be enabled for the abbrev expansions to happen
;;   automatically in future. But in case you want to explicitly do an abbrev
;;   expansion (even when `abbrev-mode' is nil), you can call the
;;   `expand-abbrev' command which is bound by default to
;;      C-x ', C-x a ' and C-x a e
;;
;; http://www.star.bris.ac.uk/bjm/emacs-tips.html#sec-1-17
;; Usage example: Now you can type the abbreviation you want, followed by
;; `C-x a -' and you will be prompted for the expanded text.
;;   e.g. fn C-x a - function
;;
;; To remove an abbrev definition, give a negative argument to the abbrev
;; definition command: C-u - C-x a g or C-u - C-x a l.
