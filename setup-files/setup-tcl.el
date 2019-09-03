;; Time-stamp: <2019-08-22 12:09:05 kmodi>

;; Tcl

(use-package tcl
  :mode (("\\.tcl\\'" . tcl-mode)
         ("\\.tcons\\'" . tcl-mode)
         ("\\.svcf\\'" . tcl-mode)
         ("\\.cer\\'" . tcl-mode))
  :config
  (progn
    (setq tcl-indent-level 2)

    (defun modi/tcl-mode-customization ()
      "My customization for `tcl-mode'."
      (electric-indent-mode -1))
    (add-hook 'tcl-mode-hook #'modi/tcl-mode-customization)))


(provide 'setup-tcl)
