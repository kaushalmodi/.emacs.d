;; Time-stamp: <2017-04-14 23:15:22 kmodi>

;; Nim
;; https://github.com/nim-lang/nim-mode

(use-package nim-mode
  :ensure t
  :mode (("\\.nim\\'" . nim-mode))
  :config
  (progn
    (with-eval-after-load 'smart-compile
      (add-to-list 'smart-compile-alist
                   '(nim-mode . "nim c --verbosity:0 %f")))))


(provide 'setup-nim)
