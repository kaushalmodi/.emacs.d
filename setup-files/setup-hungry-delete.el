;; Time-stamp: <2014-08-13 15:05:48 kmodi>

;; Hungry Delete

(req-package hungry-delete
  :config
  (progn
    (setq hungry-delete-chars-to-skip " \t\r\f\v")
    (global-hungry-delete-mode nil)
    (add-hook 'prog-mode-hook 'hungry-delete-mode)))


(provide 'setup-hungry-delete)
