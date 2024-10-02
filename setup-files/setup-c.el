;; Time-stamp: <2024-10-02 10:39:14 kmodi>

;; C/C++

(use-package cc-mode
  :mode (("\\.pss\\'" . c++-mode)) ;Portable Stimulus files
  :config
  (progn
    (setq-default c-basic-offset 4)))


(provide 'setup-c)
