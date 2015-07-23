;; Time-stamp: <2015-07-23 10:08:59 kmodi>

;; Which Key
;; https://github.com/justbur/emacs-which-key

(use-package which-key
  :defer 15
  :config
  (progn
    (setq which-key-popup-type 'side-window) ; default
    ;; (setq which-key-popup-type 'minibuffer)

    (setq which-key-key-replacement-alist
          '(("<\\([[:alnum:]-]+\\)>" . "\\1")
            ("left"  . "◀")
            ("right" . "▶")
            ("up"    . "▲")
            ("down"  . "▼")
            ("next"  . "PgDn")
            ("prior" . "PgUp")))

    (setq which-key-key-based-description-replacement-alist
          '(("C-x 8"   . "unicode")
            ("C-x a"   . "abbrev/expand")
            ("C-x r"   . "rect/reg")
            ("C-x w"   . "hi-lock-map")
            ("C-c /"   . "engine-mode-map")
            ("C-c C-v" . "org-babel")
            ("C-x 8 0" . "ZWS")))

    (with-eval-after-load 'setup-symbola
      (if font-symbola-p
          (setq which-key-description-replacement-alist
                '(("Prefix Command" . "prefix")
                  ("which-key-show-next-page" . "wk next pg")
                  ;; Hide the "calc-" prefixes when listing keys for M-x calc
                  ("\\`calc-" . "🖩")))
        (setq which-key-description-replacement-alist
              '(("Prefix Command" . "prefix")
                ("which-key-show-next-page" . "wk next pg")
                ;; Hide the "calc-" prefixes when listing keys for M-x calc
                ("\\`calc-" . "")))))

    ;; Paging
    (dolist (prefix '("C-x" "C-c"))
      (dolist (paging-key '("<next>" "<prior>")) ; Pg Down, Pg Up
        (define-key which-key-mode-map
          (kbd (concat prefix " " paging-key)) #'which-key-show-next-page)))

    (which-key-mode 1)))


(provide 'setup-which-key)
