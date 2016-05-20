;; Time-stamp: <2016-05-19 22:45:12 kmodi>

;; Smart M-x (smex)
;; https://github.com/nonsequitur/smex/

(use-package smex
  :bind (("C-c M-x" . execute-extended-command)) ; old M-x
  :bind (:map modi-mode-map
         ("M-X" . smex-major-mode-commands))
  :init
  (progn
    (smex-initialize)
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
