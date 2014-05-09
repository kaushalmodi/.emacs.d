;; Time-stamp: <2014-05-09 11:25:08 kmodi>

;; IRegister (Interactive Register)
;; https://github.com/atykhonov/iregister.el

(require 'iregister)

;; Workaround to save the iregister copied text to clipboard as well
;; Source: https://github.com/atykhonov/iregister.el/issues/1
(defun demi/iregister-point-or-kill-ring-save-text-to-register (start end)
  (interactive "r")
  (kill-ring-save start end)
  (iregister-point-or-text-to-register))


(setq setup-iregister-loaded t)
(provide 'setup-iregister)
