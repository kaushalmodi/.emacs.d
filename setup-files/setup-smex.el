;; Time-stamp: <2014-01-13 17:47:22 kmodi>

;; Smart M-x (smex)
;; Source: https://github.com/nonsequitur/smex/

;; Command help
;; `C-h f` while Smex is active, runs describe-function on the currently selected command.
;; `M-.` jumps to the definition of the selected command.
;; `C-h w` shows the key bindings for the selected command. (Via where-is.)

(require 'smex)

(smex-initialize)


(setq setup-smex-loaded t)
(provide 'setup-smex)
