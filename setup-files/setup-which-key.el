;; Time-stamp: <2015-07-15 14:30:33 kmodi>

;; Which Key
;; https://github.com/justbur/emacs-which-key

(use-package which-key
  :defer 15
  :config
  (progn
    (setq which-key-popup-type 'minibuffer) ; default
    ;; (setq which-key-popup-type 'side-window)

    (setq which-key-key-replacement-alist
          '(("<\\(\\(C-\\|M-\\)*.+\\)>" . "\\1")
            ("left"  . "◀")
            ("right" . "▶")
            ("up"    . "▲")
            ("down"  . "▼")))

    (setq which-key-special-keys '("SPC" "TAB" "RET" "ESC" "DEL"))

    (setq which-key-key-based-description-replacement-alist
          '(("C-x 8" . "unicode")
            ("C-x a" . "abbrev/expand")
            ("C-x r" . "rect/reg")
            ("C-x w" . "hi-lock-map")
            ("C-c /" . "engine-mode-map")))

    (which-key-mode 1)))


(provide 'setup-which-key)
