;; Time-stamp: <2015-07-21 17:19:59 kmodi>

;; Which Key
;; https://github.com/justbur/emacs-which-key

(use-package which-key
  :defer 15
  :config
  (progn
    (setq which-key-popup-type 'minibuffer) ; default
    ;; (setq which-key-popup-type 'side-window)

    (setq which-key-sort t) ; sort keys alphabetically

    (setq which-key-key-replacement-alist
          '(("<\\(\\(C-\\|M-\\)*.+\\)>" . "\\1")
            ("left"  . "◀")
            ("right" . "▶")
            ("up"    . "▲")
            ("down"  . "▼")
            ("next"  . "PgDn")
            ("prior" . "PgUp")))

    (setq which-key-special-keys '("SPC" "TAB" "RET" "ESC" "DEL"))

    (setq which-key-key-based-description-replacement-alist
          '(("C-x 8"   . "unicode")
            ("C-x a"   . "abbrev/expand")
            ("C-x r"   . "rect/reg")
            ("C-x w"   . "hi-lock-map")
            ("C-c /"   . "engine-mode-map")
            ("C-c C-v" . "org-babel")
            ("C-x 8 0" . "ZWS")))

    ;; Paging
    (setq which-key-paging-prefixes '("C-x" "C-c"))
    (setq which-key-paging-key "<next>") ; Pg Down

    (which-key-mode 1)))


(provide 'setup-which-key)
