;; Time-stamp: <2015-06-10 09:01:15 kmodi>

;; Smart M-x (smex)
;; https://github.com/nonsequitur/smex/

(use-package smex
  :init
  (progn
    (smex-initialize)
    (bind-keys
     :map modi-mode-map
      ("M-x" . smex)
      ("M-X" . smex-major-mode-commands))
    (bind-key "C-c C-c M-x" #'execute-extended-command) ; old M-x
    ;; (key-chord-define-global ";'" #'smex) ; Alternative for `M-x'
    ))

(provide 'setup-smex)

;; Useful bindings while smex is active
;;
;; |---------+------------------------------------------------------------|
;; | Binding | Description                                                |
;; |---------+------------------------------------------------------------|
;; | C-h f   | Runs `describe-function' on the currently selected command |
;; | M-.     | Jump to the definition of the selected command             |
;; | C-h w   | Show the key bindings for the selected command             |
;; |---------+------------------------------------------------------------|
