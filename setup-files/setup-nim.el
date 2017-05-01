;; Time-stamp: <2017-05-01 17:28:23 kmodi>

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
      (add-hook 'nim-mode-hook #'nimsuggest-mode))))


(provide 'setup-nim)
