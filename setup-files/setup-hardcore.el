;; Time-stamp: <2014-08-13 10:45:34 kmodi>

;; Hardcore mode

(req-package hardcore-mode
  :pre-load
  (progn
    (setq too-hardcore-backspace t
          too-hardcore-return    t))
  :config
  (progn
    (global-hardcore-mode)))


(provide 'setup-hardcore)
