;; Time-stamp: <2015-02-18 17:06:38 kmodi>

;; Printing
;; http://www.emacswiki.org/emacs/PsPrintPackage-23

(req-package ps-print
  :config
  (progn
    ;; Print configuration
    (setq ps-paper-type             'letter)
    (setq ps-print-color-p          'black-white)
    (setq ps-font-family            'Courier)
    (setq ps-font-size              8.5) ; default = 8.5
    (setq ps-landscape-mode         nil)
    (setq ps-number-of-columns      1)

    ;; Header configuration
    (setq ps-print-header           1)
    (setq ps-print-header-frame     nil)
    (setq ps-print-only-one-header  1)
    (setq ps-header-font-family     'Courier)
    (setq ps-header-title-font-size 8.5)
    (setq ps-header-font-size       8.0)
    (setq ps-header-lines           2 )
    ;; |-----------------+------------------------------------------------------------|
    ;; | ps-header-lines | Description                                                |
    ;; |-----------------+------------------------------------------------------------|
    ;; |               1 | Show buffer name, page number                              |
    ;; |               2 | Show buffer name, page number, path to file, date          |
    ;; |               3 | Show buffer name, page number, path to file, date and time |
    ;; |-----------------+------------------------------------------------------------|

    ;; Line number configuration
    (setq ps-line-number           1)
    (setq ps-line-number-font      'Courier)
    (setq ps-line-number-font-size 8.0)
    (setq ps-line-number-color     '(0.65 0.65 0.65)) ; gray
    ;; ps-line-number-color '(0.0 0.0 0.0) ; black
    ;; ps-line-number-color '(1.0 1.0 1.0) ; white

    (when (executable-find "ps2pdf")
      (defun modi/pdf-print-buffer-with-faces (&optional filename)
        "Print file in the current buffer as pdf, including font, color, and
underline information.  This command works only if you are using a window system,
so it has a way to determine color values.

C-u COMMAND prompts user where to save the Postscript file (which is then
converted to PDF at the same location."
        (interactive (list (if current-prefix-arg
                               (ps-print-preprint 4)
                             (concat (file-name-sans-extension (buffer-file-name))
                                     ".ps"))))
        (ps-print-with-faces (point-min) (point-max) filename)
        (shell-command (concat "ps2pdf " filename))
        (delete-file filename)
        (message "Deleted %s" filename)
        (message "Wrote %s" (concat (file-name-sans-extension filename) ".pdf")))
      (bind-to-modi-map "p" modi/pdf-print-buffer-with-faces))

    ;; Print to printer defined by env var `PRINTER'
    (bind-to-modi-map "P" ps-print-buffer-with-faces)))


(provide 'setup-print)

;; To export the file as a syntax hightlighted .ps from emacs
;;   `C-u M-x ps-print-buffer-with-faces` and save that file with any name when
;; prompted for the output file name
;; !! If you don't use <C-u> in the above command, emacs will print
;;    it directly to the default printer !!
