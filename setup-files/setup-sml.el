;; Time-stamp: <2016-08-09 08:16:06 kmodi>

;; Standard ML

(use-package sml-mode
  :ensure t
  :mode (("\\.sml\\'" . sml-mode))
  :config
  (progn
    ;; Undefine all the default abbrevs defined in `sml-mode.el'.
    ;; I cannot use `clear-abbrev-table' because that will clear out my
    ;; personally defined abbrevs too!
    (dolist (abbrev '("let"
                      "if"
                      "local"
                      "case"
                      "signature"
                      "structure"
                      "functor"
                      "datatype"
                      "abstype"
                      "struct"
                      "sig"
                      "val"
                      "fn"
                      "fun"))
      (define-abbrev sml-mode-abbrev-table abbrev nil))

    (setcdr (assoc "andalso" sml-font-lock-symbols-alist) "&")
    (setcdr (assoc "orelse" sml-font-lock-symbols-alist) "|")

    (defun modi/sml-mode-hook-fn ()
      "My customizations for `sml-mode'."
      ;; ;; In SML, it is perfectly fine to have the statements not ending in
      ;; ;; semicolons as below (in files, *not* in REPL!):
      ;; ;;   val x = 34
      ;; ;; But when doing so, newline with auto-indentation does not work
      ;; ;; correctly. Newline + auto-indentation works fine only if the statements
      ;; ;; are ended in semi-colons. So `electric-indent-mode will have to be
      ;; ;; disabled for this major mode.
      ;; (electric-indent-local-mode -1)
      (auto-fill-mode))
    (add-hook 'sml-mode-hook #'modi/sml-mode-hook-fn)

    ;; Do not bind M-SPC to `sml-electric-space' in `sml-mode-map'.
    ;; I prefer the default binding to `just-one-space'.
    (bind-key "M-SPC" nil sml-mode-map)))


(provide 'setup-sml)
