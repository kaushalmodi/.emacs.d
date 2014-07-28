;; Time-stamp: <2014-07-28 00:44:25 kmodi>

;; Hungry Delete

(require 'hungry-delete)

(setq my-global-hungry-delete-mode t)

;; `turn-on-hungry-delete-mode' is defined in the hungry-mode package
(defun turn-off-hungry-delete-mode()
  (interactive)
  (hungry-delete-mode -1))

;; (global-hungry-delete-mode nil)
;; (add-hook 'activate-mark-hook   'turn-off-hungry-delete-mode)
;; (add-hook 'deactivate-mark-hook 'turn-on-hungry-delete-mode)

(if my-global-hungry-delete-mode
    (if delete-selection-mode
        (progn
          (global-hungry-delete-mode nil)
          (add-hook 'activate-mark-hook   'turn-off-hungry-delete-mode)
          (add-hook 'deactivate-mark-hook 'turn-on-hungry-delete-mode))
      (progn
        (global-hungry-delete-mode t)
        (remove-hook 'activate-mark-hook   'turn-off-hungry-delete-mode)
        (remove-hook 'deactivate-mark-hook 'turn-on-hungry-delet-mode)))
  (global-hungry-delete-mode nil))


(setq setup-hungry-delete-loaded t)
(provide 'setup-hungry-delete)
