;; Time-stamp: <2024-10-17 18:06:19 kmodi>

;; C/C++

(use-package cc-mode
  :mode (("\\.pss\\'" . c++-mode)         ;Portable Stimulus files
         ("\\.psf\\'" . c++-mode)
         ("\\.sln\\'" . c++-mode))
  :config
  (progn
    (setq-default c-basic-offset 3)))


(provide 'setup-c)
