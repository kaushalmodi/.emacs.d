;; Time-stamp: <2015-07-15 12:09:48 kmodi>

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

    (which-key-mode 1)))


(provide 'setup-which-key)
