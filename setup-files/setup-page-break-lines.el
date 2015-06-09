;; Time-stamp: <2015-06-09 14:55:27 kmodi>

;; Show actual lines instead of the page break char ^L
;; https://github.com/purcell/page-break-lines

(use-package page-break-lines
  :config
  (progn
    (setq page-break-lines-modes '(emacs-lisp-mode
                                   lisp-mode
                                   scheme-mode
                                   compilation-mode
                                   outline-mode
                                   help-mode
                                   text-mode
                                   fundamental-mode
                                   special-mode))

    ;; Fix the issue of page break rule wrapping across multiple lines
    ;; - Above fix doesn't work -- Tue Jun 09 14:03:38 EDT 2015 - kmodi
    ;; (set-fontset-font "fontset-default"
    ;;                   (cons page-break-lines-char page-break-lines-char)
    ;;                   (face-attribute 'default :family))

    ;; Calling `global-page-break-lines-mode' will enable `page-break-mode'
    ;; automatically in all the modes listed in `page-break-lines-modes' var
    (global-page-break-lines-mode)))


(provide 'setup-page-break-lines)
