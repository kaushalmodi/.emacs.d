;; Time-stamp: <2014-08-13 11:25:26 kmodi>

(req-package page-break-lines
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
