;; Time-stamp: <2015-02-23 11:42:33 kmodi>

;; Hardcore mode

(use-package hardcore-mode
  :pre-load
  (progn
    (setq too-hardcore-backspace t
          too-hardcore-return    t))
  :config
  (progn
    (global-hardcore-mode)))


(provide 'setup-hardcore)
