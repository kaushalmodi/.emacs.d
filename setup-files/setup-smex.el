;; Time-stamp: <2015-02-23 11:00:29 kmodi>

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
    (when (featurep 'key-chord)
      (key-chord-define-global ";'" #'smex)))) ; Alternative for `M-x'


(provide 'setup-smex)

;; `C-h f` while Smex is active, runs describe-function on the currently selected command.
;; `M-.`   jumps to the definition of the selected command.
;; `C-h w` shows the key bindings for the selected command. (Via where-is.)
