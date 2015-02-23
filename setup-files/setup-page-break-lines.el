;; Time-stamp: <2015-02-23 11:42:33 kmodi>

(use-package page-break-lines
  :config
  (progn
    (setq page-break-lines-modes
          '(emacs-lisp-mode
            lisp-mode
            scheme-mode
            compilation-mode
            outline-mode
            help-mode
            text-mode))
    ;; Calling `global-page-break-lines-mode' will enable `page-break-mode'
    ;; automatically in all the modes listed in `page-break-lines-modes' var
    (global-page-break-lines-mode)))


(provide 'setup-page-break-lines)
