;; Time-stamp: <2015-01-16 11:06:43 kmodi>

;; Hungry Delete
;; https://github.com/nflath/hungry-delete

(req-package hungry-delete
  :config
  (progn
    (setq hungry-delete-chars-to-skip " \t\r\f\v")
    (global-hungry-delete-mode nil)
    (add-hook 'prog-mode-hook 'hungry-delete-mode)))


(provide 'setup-hungry-delete)
