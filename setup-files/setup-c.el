;; Time-stamp: <2023-01-20 16:54:21 kmodi>

;; C/C++

(use-package cc-mode
  :mode (("\\.pss\\'" . c-or-c++-mode)) ;Perspec Portable Stimulus files
  :config
  (progn
    (setq-default c-basic-offset 4)))


(provide 'setup-c)
