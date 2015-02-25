;; Time-stamp: <2015-02-25 12:38:22 kmodi>

;; Hardcore mode

(use-package hardcore-mode
  :pre-load
  (progn
    (setq too-hardcore-backspace t)
    (setq too-hardcore-return    t))
  :config
  (progn
    (global-hardcore-mode 1)))


(provide 'setup-hardcore)
