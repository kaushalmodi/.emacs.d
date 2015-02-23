;; Time-stamp: <2015-02-23 11:42:32 kmodi>

;; Hungry Delete
;; https://github.com/nflath/hungry-delete

(use-package hungry-delete
  :config
  (progn
    (setq hungry-delete-chars-to-skip " \t\r\f\v")
    (global-hungry-delete-mode nil)
    (add-hook 'prog-mode-hook 'hungry-delete-mode)))


(provide 'setup-hungry-delete)
