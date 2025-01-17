;; Time-stamp: <2025-01-17 11:59:43 kmodi>

;; Tcl

(use-package tcl
  :mode (("\\.tcl\\'" . tcl-mode)
         ("\\.tcons\\'" . tcl-mode)
         ("\\.svcf\\'" . tcl-mode)
         ("\\.cer\\'" . tcl-mode)
         ("vm[a-z]+\\.cmds\\'" . tcl-mode))
  :config
  (progn
    (setq tcl-indent-level 2)

    (defun modi/tcl-mode-customization ()
      "My customization for `tcl-mode'."
      (electric-indent-mode -1))
    (add-hook 'tcl-mode-hook #'modi/tcl-mode-customization)))


(provide 'setup-tcl)
