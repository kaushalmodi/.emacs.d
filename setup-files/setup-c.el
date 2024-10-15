;; Time-stamp: <2024-10-15 11:27:12 kmodi>

;; C/C++

(use-package cc-mode
  :mode (("\\.pss\\'" . c++-mode)) ;Portable Stimulus files
  :config
  (progn
    (setq-default c-basic-offset 3)))


(provide 'setup-c)
