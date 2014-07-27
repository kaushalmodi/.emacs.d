;; Time-stamp: <2014-07-27 00:14:47 kmodi>

;; Hungry Delete

(require 'hungry-delete)

;; (global-hungry-delete-mode)

(defun turn-on-hungry-delete-mode()
  (interactive)
  (hungry-delete-mode 1))

(defun turn-off-hungry-delete-mode()
  (interactive)
  (hungry-delete-mode -1))

(add-hook 'activate-mark-hook   'turn-off-hungry-delete-mode)
(add-hook 'deactivate-mark-hook 'turn-on-hungry-delete-mode)


(setq setup-hungry-delete-loaded t)
(provide 'setup-hungry-delete)
