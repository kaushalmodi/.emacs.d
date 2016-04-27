;; Time-stamp: <2016-04-27 14:57:35 kmodi>

;; Smart M-x (smex)
;; https://github.com/nonsequitur/smex/

(use-package smex
  :init
  (progn
    (smex-initialize)
    (bind-key "M-X" #'smex-major-mode-commands modi-mode-map)
    (bind-key "C-c M-x" #'execute-extended-command) ; old M-x
    (when (bound-and-true-p disable-pkg-ivy)
      (bind-key "M-x" #'smex modi-mode-map)
      (key-chord-define-global ";'" #'smex)))) ; alternative to `M-x'


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
