;; Time-stamp: <2014-10-23 09:01:56 kmodi>

;; IRegister (Interactive Register)
;; https://github.com/atykhonov/iregister.el

(req-package iregister
  :config
  (progn
    (bind-keys
     ;; If region is active then `iregister-point-or-text-to-register' command stores a
     ;; text to any empty register, otherwise it stores a point.
     ("M-w"     . iregister-point-or-text-to-register-kill-ring-save) ;; Replace normal copy function
     ("C-w"     . iregister-copy-to-register-kill) ;; Replace normal 'cut' function
     ;; Copy the selection and append to the latest register
     ("C-x r a" . iregister-append-to-latest-register)
     ;; Delete the selection and append to the latest register
     ("C-x r A" . iregister-append-to-latest-register-delete)
     ("M-n"     . iregister-jump-to-next-marker)
     ("M-p"     . iregister-jump-to-previous-marker))
    ;; Assuming that there are already stored some texts (by means of `copy-to-register'
    ;; or `iregister-copy-to-register' command) in the registers. Execute
    ;; `iregister-text' and the minibuffer will display the text stored in some
    ;; register.
    ;; Key bindings when the `iregister-text minibuffer is active:
    ;;   RET        - The selected text will be inserted
    ;;   l          - View the latest text stored in the registers
    ;;   n          - View next text previously stored in the registers
    ;;   p          - View previous text previously stored in the registers
    ;;   d          - Delete current text from the register
    ;;   q or `C-g' - To quit from the minibuffer
    ;;   a          - Append the selected text to the current text registry
    ;;   A          - Prepend the selected text to the current text registry
    (bind-to-modi-map "i" iregister-latest-text)))


(provide 'setup-iregister)
