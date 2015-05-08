;; Time-stamp: <2015-05-08 08:19:20 kmodi>

;; IRegister (Interactive Register)
;; https://github.com/atykhonov/iregister.el

(defalias 'my/iregister-copy          'iregister-point-or-text-to-register-kill-ring-save)
(defalias 'my/iregister-cut           'iregister-copy-to-register-kill)
(defalias 'my/iregister-copy-append   'iregister-append-to-latest-register)
(defalias 'my/iregister-delete-append 'iregister-append-to-latest-register-delete)

(use-package iregister
  :config
  (progn

    (defun my/iregister-copy-append-sep (start end &optional separator)
      (interactive "r\nsSeparator: ")
      (if (region-active-p)
          (let ((selection (buffer-substring-no-properties start end)))
            (if iregister-last-used-register
                (with-temp-buffer
                  (if (string-match "\\n" separator)
                      (newline)
                    (insert separator))
                  (insert selection)
                  (append-to-register iregister-last-used-register
                                      (point-min) (point-max)))
              (message "No registers has been used within iregister.el."))
            (deactivate-mark))
        (message "!Region is not active.")))

    (defun my/iregister-delete-append-sep (start end &optional separator)
      (interactive "r\nsSeparator: ")
      (if (region-active-p)
          (let ((selection (buffer-substring-no-properties start end)))
            (if iregister-last-used-register
                (progn
                  (with-temp-buffer
                    (if (string-match "\\n" separator)
                        (newline)
                      (insert separator))
                    (insert selection)
                    (append-to-register iregister-last-used-register
                                        (point-min) (point-max)))
                  (delete-region start end))
              (message "No registers has been used within iregister.el."))
            (deactivate-mark))
        (message "!Region is not active.")))

    (defhydra hydra-append (:color pink)
      "append"
      ;; Copy the selection and append to the latest register
      ("."  my/iregister-copy-append                         "Rcopy-nil-r")
      ("s"  (lambda (beg end)
              (interactive "r")
              (my/iregister-copy-append-sep beg end " "))    "Rcopy-SPC-r")
      ("n"  (lambda (beg end)
              (interactive "r")
              (my/iregister-copy-append-sep beg end "\n"))   "Rcopy-newl-r")
      ;; Delete the selection and append to the latest register
      ("x." my/iregister-delete-append                       "Rdel-nil-r")
      ("xs" (lambda (beg end)
              (interactive "r")
              (my/iregister-delete-append-sep beg end " "))  "Rdel-SPC-r")
      ("xn" (lambda (beg end)
              (interactive "r")
              (my/iregister-delete-append-sep beg end "\n")) "Rdel-newl-r")
      ("q"  nil                                              "cancel" :color blue))

    (bind-keys
     ;; If region is active then `iregister-point-or-text-to-register' command
     ;; stores a text to any empty register, otherwise it stores a point.
     ("M-w"     . my/iregister-copy) ; Replace normal copy function
     ("C-w"     . my/iregister-cut) ; Replace normal 'cut' function
     ("C-x r a" . hydra-append/body))

    (bind-to-modi-map "i" iregister-latest-text)))


(provide 'setup-iregister)

;; Assuming that some text is already stored (by means of `copy-to-register'
;; or `iregister-copy-to-register' command) in the registers, execute
;; `iregister-text' and the minibuffer will display text stored in a register.
;;
;; Key bindings when the `iregister-text' minibuffer is active:
;; |----------+--------------------------------------------------------|
;; | Binding  | Description                                            |
;; |----------+--------------------------------------------------------|
;; | RET      | The selected text will be inserted                     |
;; | l        | View the latest text stored in the registers           |
;; | n        | View next text previously stored in the registers      |
;; | p        | View previous text previously stored in the registers  |
;; | d        | Delete current text from the register                  |
;; | q or C-g | To quit from the minibuffer                            |
;; | a        | Append the selected text to the current text registry  |
;; | A        | Prepend the selected text to the current text registry |
;; |----------+--------------------------------------------------------|
