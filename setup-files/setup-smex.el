;; Time-stamp: <2014-08-13 13:30:11 kmodi>

;; Smart M-x (smex)
;; Source: https://github.com/nonsequitur/smex/

;; Command help
;; `C-h f` while Smex is active, runs describe-function on the currently selected command.
;; `M-.` jumps to the definition of the selected command.
;; `C-h w` shows the key bindings for the selected command. (Via where-is.)

(req-package smex
  :require (key-chord)
  :init
  (progn
    (smex-initialize)
    (bind-keys
     :map modi-mode-map
     ("M-x"         . smex)
     ("M-X"         . smex-major-mode-commands))
    (bind-key "C-c C-c M-x" 'execute-extended-command) ;; old M-x
    (key-chord-define-global ";'" 'smex))) ;; Alternative for `M-x'


(provide 'setup-smex)
