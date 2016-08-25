;; Time-stamp: <2016-08-25 11:44:44 kmodi>

;; Printing

(use-package ps-print
  :init
  (progn
    (bind-to-modi-map "P" #'modi/pdf-print-buffer-with-faces))
  :commands (modi/pdf-print-buffer-with-faces)
  :config
  (progn
    ;; Print configuration
    (setq ps-paper-type        'letter)      ; default = 'letter
    (setq ps-font-family       'Courier)
    (setq ps-font-size         8.5)     ; default = 8.5
    (setq ps-landscape-mode    nil)     ; default = nil (portrait)
    (setq ps-number-of-columns 1)

    ;; Color or black & white
    (defvar modi/ps-print-color-with-black-bg nil
      "When non-nil, colored postscript output will have black bg and white fg.")

    (setq ps-print-color-p 'black-white) ; default = t
    (if (and modi/ps-print-color-with-black-bg
             (eq ps-print-color-p t))
        (progn
          (setq ps-default-fg 1.0)            ; white
          (setq ps-default-bg 0.0))           ; black
      ;; The default fg and bg settings should effective only when
      ;; `ps-print-color-p' is `t'. But it is effective when that var is set to
      ;; `black-white' too.
      ;;   http://debbugs.gnu.org/cgi/bugreport.cgi?bug=24308
      ;; Below workaround fixes that.
      (setq ps-default-fg nil)
      (setq ps-default-bg nil))

    ;; Header configuration
    (setq ps-print-header t)
    (setq ps-header-lines 2)            ; 1 = Show buffer name, page number
                                        ; 2 = ^above + path to file, date
                                        ; 3 = ^above + time
    (setq ps-print-header-frame     nil)
    (setq ps-print-only-one-header  t)
    (setq ps-header-font-family     'Courier)
    (setq ps-header-title-font-size 8.5)
    (setq ps-header-font-size       8.0)

    ;; Line number configuration
    (setq ps-line-number           t)
    (setq ps-line-number-font      'Courier)
    (setq ps-line-number-font-size 8.0)
    (setq ps-line-number-color     '(0.65 0.65 0.65)) ; gray
    ;; ps-line-number-color '(0.0 0.0 0.0) ; black
    ;; ps-line-number-color '(1.0 1.0 1.0) ; white

    (when (executable-find "ps2pdf")
      (defun modi/pdf-print-buffer-with-faces (option)
        "Print file in the current buffer as pdf, including font, color, and
underline information.  This command works only if you are using a window
system, so it has a way to determine color values.

If OPTION is \\='(4), open the PDF file after generating it.

If OPTION is \\='(16), prompt the user for the Postscript file save location,
which is then converted to PDF at the same location."
        (interactive "P")
        (let* ((open-pdf (eq 4 (car option)))
               (prompt-for-ps-filename (eq 16 (car option)))
               (ps-filename (if prompt-for-ps-filename
                                (ps-print-preprint 4)
                              (concat (file-name-sans-extension (buffer-file-name))
                                      ".ps")))
               (pdf-filename (concat (file-name-sans-extension ps-filename)
                                     ".pdf")))
          (ps-print-with-faces (point-min) (point-max) ps-filename)
          (shell-command (concat "ps2pdf " ps-filename))
          (delete-file ps-filename)
          (message "Deleted %s" ps-filename)
          (message "Wrote %s" pdf-filename)
          (when open-pdf
            (find-file pdf-filename)))))))


(provide 'setup-print)

;;     M-x ps-print-buffer-with-faces -> Print to the printer defined by
;;                                       env var `PRINTER'.
;; C-u M-x ps-print-buffer-with-faces -> Export the file as a syntax
;;                                       hightlighted .ps from emacs and
;;                                       save that file with any name when
;;                                       prompted for the output file name
;;         !! If you don't use <C-u> in the above command, emacs will print
;;            it directly to the default printer !!
