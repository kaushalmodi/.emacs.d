;; Time-stamp: <2017-07-06 18:46:00 kmodi>

;; Nim
;; https://github.com/nim-lang/nim-mode

(use-package nim-mode
  :ensure t
  :mode (("\\.nim\\'" . nim-mode))
  :config
  (progn
    (with-eval-after-load 'smart-compile
      (add-to-list 'smart-compile-alist
                   '(nim-mode . "nim c --verbosity:0 %f")))

    (when (executable-find "nimsuggest")
      (setq nim-nimsuggest-path (executable-find "nimsuggest"))
      ;; Currently nimsuggest doesn't support nimscript files, so only nim-mode ..
      ;; nimsuggest will provide hints in the minibuffer using `eldoc-mode'.
      (add-hook 'nim-mode-hook #'nimsuggest-mode))

    (use-package ob-nim
      :ensure t
      :config
      (progn
        (with-eval-after-load 'setup-org
          (add-to-list 'modi/ob-enabled-languages "nim" :append))))))


(provide 'setup-nim)
