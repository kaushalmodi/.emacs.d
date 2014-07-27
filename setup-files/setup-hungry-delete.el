;; Time-stamp: <2014-07-27 13:06:13 kmodi>

;; Hungry Delete

(require 'hungry-delete)

;; (global-hungry-delete-mode)

;; `turn-on-hungry-delete-mode' is defined in the hungry-mode package

(defun turn-off-hungry-delete-mode()
  (interactive)
  (hungry-delete-mode -1))

(add-hook 'activate-mark-hook   'turn-off-hungry-delete-mode)
(add-hook 'deactivate-mark-hook 'turn-on-hungry-delete-mode)


(setq setup-hungry-delete-loaded t)
(provide 'setup-hungry-delete)
