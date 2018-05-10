;; Time-stamp: <2018-05-10 09:44:07 kmodi>

;; Nim
;; https://github.com/nim-lang/nim-mode

(use-package nim-mode
  :ensure t
  :mode (("\\.nim\\'" . nim-mode))
  :config
  (progn
    (key-chord-define nim-mode-map "??" #'devdocs-lookup)
    (with-eval-after-load 'smart-compile
      (add-to-list 'smart-compile-alist
                   '(nim-mode . "nim c --verbosity:0 %f")))

    (when (executable-find "nimsuggest")
      (setq nim-nimsuggest-path (executable-find "nimsuggest"))
      ;; Currently nimsuggest doesn't support nimscript files, so only nim-mode ..
      ;; nimsuggest will provide hints in the minibuffer using `eldoc-mode'.
      (add-hook 'nim-mode-hook #'nimsuggest-mode)
      ;; (remove-hook 'nim-mode-hook #'nimsuggest-mode)
      )

    (use-package ob-nim
      :ensure t
      :config
      (progn
        (with-eval-after-load 'setup-org
          (add-to-list 'modi/ob-enabled-languages "nim" :append))))))


(provide 'setup-nim)
