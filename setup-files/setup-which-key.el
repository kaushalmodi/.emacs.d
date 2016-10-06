;; Time-stamp: <2016-10-06 13:31:24 kmodi>

;; Which Key
;; https://github.com/justbur/emacs-which-key

(use-package which-key
  :defer 10
  :config
  (progn
    (>=e "26.0"
        ;; https://github.com/justbur/emacs-which-key/issues/146
        ;; http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=b8fd71d5709650c1aced92c772f70595c51881d2
        ;; `display-buffer-in-major-side-window' renamed to `window--make-major-side-window' in emacs 26+
        (defalias 'display-buffer-in-major-side-window 'window--make-major-side-window))

    (setq which-key-popup-type 'side-window) ; default
    ;; (setq which-key-popup-type 'minibuffer)

    ;; Replacements for how KEY is replaced when which-key displays
    ;;   KEY → FUNCTION
    ;; Eg: After "C-c", display "right → winner-redo" as "▶ → winner-redo"
    (setq which-key-key-replacement-alist
          '(("<\\([[:alnum:]-]+\\)>" . "\\1")
            ("left"                . "◀")
            ("right"               . "▶")
            ("up"                  . "▲")
            ("down"                . "▼")
            ("delete"              . "DLT") ; delete key
            ("\\`DEL\\'"             . "BS") ; backspace key
            ("next"                . "PgDn")
            ("prior"               . "PgUp")))

    ;; Replacements for how part or whole of FUNCTION is replaced when
    ;; which-key displays
    ;;   KEY → FUNCTION
    ;; Eg: After "d" in `calc', display "6 → calc-hex-radix" as "6 → 🖩hex-radix"
    (setq which-key-description-replacement-alist
          '(("Prefix Command" . "prefix")
            ("which-key-show-next-page" . "wk next pg")
            ("\\`calc-" . "") ; Hide "calc-" prefixes when listing M-x calc keys
            ("/body\\'" . "") ; Remove display the "/body" portion of hydra fn names
            ("\\`artist-select-op-" . "") ; Make artist-mode function names less verbose
            ("\\`artist-select-" . "sel-")
            ("\\`artist-toggle-" . "toggle-")
            ("modi/" . "m/") ; The car is intentionally not "\\`modi/" to cover
                                        ; cases like `hydra-toggle/modi/..'.
            ("\\`hydra-" . "+h/")
            ("\\`org-babel-" . "ob/")))
    ;; Use cool unicode characters if available
    (with-eval-after-load 'setup-font-check
      (when font-symbola-p
        (add-to-list 'which-key-description-replacement-alist '("\\`calc-" . "🖩"))))

    ;; Change what string to display for a given *complete* key binding
    ;; Eg: After "C-x", display "8 → +unicode" instead of "8 → +prefix"
    (which-key-add-key-based-replacements
      "C-x 8"   "unicode"
      "C-x a"   "abbrev/expand"
      "C-x r"   "rect/reg"
      "C-c /"   "engine-mode-map"
      "C-c C-v" "org-babel"
      "C-x 8 0" "ZWS")

    ;; List of "special" keys for which a KEY is displayed as just K but with
    ;; "inverted video" face.
    (setq which-key-special-keys '("SPC"
                                   "TAB"
                                   "RET"
                                   "DLT" ; delete key
                                   "BS" ; backspace key
                                   "ESC"))

    ;; Highlight certain commands
    (defface modi/wk-highlight-modi-face
      '((t . (:inherit which-key-command-description-face :foreground "indian red")))
      "Face for highlighting commands starting with \"modi/\".")

    (setq which-key-highlighted-command-list
          '(("\\`hydra-" . which-key-group-description-face)
            ("\\`modi/" . modi/wk-highlight-modi-face)
            ;; Highlight using the default `which-key-highlighted-command-face'
            "\\(rectangle-\\)\\|\\(-rectangle\\)"
            "\\`org-"))

    (which-key-mode 1)))


(provide 'setup-which-key)
