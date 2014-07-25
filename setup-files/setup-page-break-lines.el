;; Time-stamp: <2014-07-24 17:11:16 kmodi>

(require 'page-break-lines)

(setq page-break-lines-modes
  '(emacs-lisp-mode lisp-mode scheme-mode compilation-mode
                    outline-mode help-mode text-mode))
;; Calling `global-page-break-lines-mode' will enable `page-break-mode'
;; automatically in all the modes listed in `page-break-lines-modes' var
(global-page-break-lines-mode)


(setq setup-page-break-lines-loaded t)
(provide 'setup-page-break-lines)
