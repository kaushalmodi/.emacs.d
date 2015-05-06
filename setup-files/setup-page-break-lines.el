;; Time-stamp: <2015-05-06 11:50:36 kmodi>

;; Show actual lines instead of the page break char ^L
;; https://github.com/purcell/page-break-lines

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
            text-mode
            fundamental-mode
            special-mode))
    ;; Calling `global-page-break-lines-mode' will enable `page-break-mode'
    ;; automatically in all the modes listed in `page-break-lines-modes' var
    (global-page-break-lines-mode)))


(provide 'setup-page-break-lines)
