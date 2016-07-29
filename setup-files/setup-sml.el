;; Time-stamp: <2016-07-28 23:19:02 kmodi>

;; Standard ML

(use-package sml-mode
  :ensure t
  :mode (("\\.sml\\'" . sml-mode))
  :config
  (progn
    (defun modi/sml-mode-hook-fn ()
      "Disable `electric-indent-mode' only locally."
      ;; In SML, it is perfectly fine to have the statements not ending in
      ;; semicolons as below (in files, *not* in REPL!):
      ;;   val x = 34
      ;; But when doing so, newline with auto-indentation does not work
      ;; correctly. Newline + auto-indentation works fine only if the statements
      ;; are ended in semi-colons. So `electric-indent-mode will have to be
      ;; disabled for this major mode.
      (electric-indent-local-mode -1))
    (add-hook 'sml-mode-hook #'modi/sml-mode-hook-fn)))


(provide 'setup-sml)
