;; Time-stamp: <2021-06-01 14:24:43 kmodi>

;; Nim
;; https://github.com/nim-lang/nim-mode

(use-package nim-mode
  :ensure t
  :mode (("\\.nim.?\\'" . nim-mode))
  :config
  (progn
    (bind-chords
     :map nim-mode-map
     ("??" . devdocs-lookup))
    (with-eval-after-load 'smart-compile
      (add-to-list 'smart-compile-alist
                   ;; Devel build of nim is needed for --nep1:on
                   ;; switch support.
                   '(nim-mode . "nim c --verbosity:0 --nep1:on %f")))

    ;; (when (executable-find "nimsuggest")
    ;;   (setq nim-nimsuggest-path (executable-find "nimsuggest"))
    ;;   ;; Currently nimsuggest doesn't support nimscript files, so only
    ;;   ;; nim-mode ..  nimsuggest will provide hints in the minibuffer
    ;;   ;; using `eldoc-mode'.
    ;;   (add-hook 'nim-mode-hook #'nimsuggest-mode)
    ;;   ;; (remove-hook 'nim-mode-hook #'nimsuggest-mode)
    ;;   )

    ;; The "pretty" triple quotes are not actually pretty.
    (setq nim-pretty-triple-double-quotes nil)

    (with-eval-after-load 'nim-syntax
      (setq nim-font-lock-keywords-extra
        `(;; export properties
          (,(nim-rx
             line-start (1+ " ")
             (? "case" (+ " "))
             (group
              (or identifier quoted-chars) "*"
              (? (and "[" word "]"))
              (0+ (and "," (? (0+ " "))
                       (or identifier quoted-chars) "*")))
             (0+ " ") (or ":" "{." "=") (0+ nonl)
             line-end)
           (1 'nim-font-lock-export-face))
          ;; Number literal
          (,(nim-rx nim-numbers)
           (0 'nim-font-lock-number-face))
          ;; Highlight identifier enclosed by "`"
          (nim-backtick-matcher
           (10 font-lock-constant-face prepend))

          ;; Thu Jun 13 23:09:08 EDT 2019 - kmodi
          ;; Commenting out the fontification of nim-format-$-matcher,
          ;; else it messes up the fontification of doc strings,
          ;; multi-line strings, etc when "$1" present in those.
          ;; Highlight $# and $[0-9]+ inside string
          ;; (nim-format-$-matcher 0 font-lock-preprocessor-face prepend)

          ;; Highlight word after ‘is’ and ‘distinct’
          (,(nim-rx " " (or "is" "distinct") (+ " ")
                    (group identifier))
           (1 font-lock-type-face))
          ;; pragma
          (nim-pragma-matcher . (0 'nim-font-lock-pragma-face)))))

    (use-package ob-nim
      :ensure t
      :config
      (progn
        (with-eval-after-load 'setup-org
          (add-to-list 'modi/ob-enabled-languages "nim" :append))))))

;; https://github.com/yuutayamada/nim-emacs-module
(use-package nim-emacs-module
  :load-path "misc/nim-emacs-module/extra/")


(provide 'setup-nim)
