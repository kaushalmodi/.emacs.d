;; Time-stamp: <2014-01-28 18:20:18 kmodi>

;; Printing
;; Source: http://www.opensource.apple.com/source/emacs/emacs-41/emacs/lisp/ps-print.el
;; http://www.emacswiki.org/emacs/PsPrintPackage-23

;; Don't set the fonts if emacs is launched in terminal mode or no-window mode,
;; using "emacs -nw". In that case the value of `window-system' is nil.
;; Source: http://stackoverflow.com/questions/5795451/how-to-detect-that-emacs-is-in-terminal-mode
(if window-system
    (set-fontset-font nil '(#x0250 . #x02af) (font-spec :family "DejaVu Sans Mono")))

(setq ps-paper-type 'letter
      ps-print-color-p 'black-white
      ;; ps-multibyte-buffer 'bdf-font ; default = nil
      ps-font-family 'Courier
      ps-font-size 8.5 ; default = 8.5
      ps-landscape-mode nil
      ps-number-of-columns 1
      ps-print-header 1
      ps-print-header-frame nil
      ps-print-only-one-header 1
      ps-header-font-family 'Courier
      ps-header-title-font-size 8.5
      ps-header-font-size 8.0
      ;; ps-header-lines 1 ; show only buffer name and page number
      ps-header-lines 2 ; show buffer name, page number, path to file, date
      ;; ps-header-lines 3 ; show buffer name, page number, path to file, date and time
      ps-line-number 1
      ps-line-number-font 'Courier
      ps-line-number-font-size 8.0
      ;; ps-line-number-color '(0.0 0.0 0.0) ; black
      ;; ps-line-number-color '(1.0 1.0 1.0) ; white
      ps-line-number-color '(0.65 0.65 0.65) ; gray
      )

(provide 'setup-print)
