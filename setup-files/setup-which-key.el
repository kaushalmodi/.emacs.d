;; Time-stamp: <2017-05-09 16:24:59 kmodi>

;; Which Key
;; https://github.com/justbur/emacs-which-key

(use-package which-key
  :defer 10
  :config
  (progn
    (setq which-key-popup-type 'side-window) ; default
    ;; (setq which-key-popup-type 'minibuffer)

    (setq which-key-replacement-alist
          '(
            ;; Replacements for how part or whole of FUNCTION is replaced when
            ;; which-key displays
            ;;   KEY → FUNCTION
            ;; Eg: After "d" in `calc', display "6 → calc-hex-radix" as "6 → 🖩hex-radix"
            ((nil . "Prefix Command")           . (nil . "prefix"))
            ((nil . "which-key-show-next-page") . (nil . "wk next pg"))
            ((nil . "\\`calc-")                  . (nil . "")) ; Hide "calc-" prefixes when listing M-x calc keys
            ((nil . "/body\\'")                  . (nil . "")) ; Remove display the "/body" portion of hydra fn names
            ((nil . "\\`artist-select-op-")      . (nil . "")) ; Make artist-mode function names less verbose
            ((nil . "\\`artist-select-")         . (nil . "sel-"))
            ((nil . "\\`artist-toggle-")         . (nil . "toggle-"))
            ((nil . "modi/")                    . (nil . "m/")) ; The car is intentionally not "\\`modi/" to cover cases like `hydra-toggle/modi/..'.
            ((nil . "\\`hydra-")                 . (nil . "+h/"))
            ((nil . "\\`org-babel-")             . (nil . "ob/"))
            ;; Replacements for how KEY is replaced when which-key displays
            ;;   KEY → FUNCTION
            ;; Eg: After "C-c", display "right → winner-redo" as "▶ → winner-redo"
            (("<left>"   . nil)                 . ("◀" . nil))
            (("<right>"  . nil)                 . ("▶" . nil))
            (("<up>"     . nil)                 . ("▲" . nil))
            (("<down>"   . nil)                 . ("▼" . nil))
            (("<delete>" . nil)                 . ("DLT" . nil)) ; delete key
            (("\\`DEL\\'"  . nil)                 . ("BS" . nil)) ; backspace key
            (("<next>"   . nil)                 . ("PgDn" . nil))
            (("<prior>"  . nil)                 . ("PgUp" . nil))
            ))
    ;; Use cool unicode characters if available
    (with-eval-after-load 'setup-font-check
      (when font-symbola-p
        (add-to-list 'which-key-replacement-alist '((nil . "\\`calc-") . (nil . "🖩")))))

    ;; Change what string to display for a given *complete* key binding
    ;; Eg: After "C-x", display "8 → +unicode" instead of "8 → +prefix"
    (which-key-add-key-based-replacements
      "C-x 8"   "unicode"
      "C-x a"   "abbrev/expand"
      "C-x r"   "rectangle/register/bookmark"
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
    (defface modi/which-key-highlight-2-face
      '((t . (:inherit which-key-command-description-face :foreground "indian red")))
      "Another face for highlighting commands in `which-key'.")

    (defface modi/which-key-highlight-3-face
      '((t . (:inherit which-key-command-description-face :foreground "DarkOrange3")))
      "Another face for highlighting commands in `which-key'.")

    (setq which-key-highlighted-command-list
          '(("\\`hydra-" . which-key-group-description-face)
            ;; Highlight using the `modi/which-key-highlight-2-face'
            ("\\`modi/" . modi/which-key-highlight-2-face)
            ;; Highlight using the `modi/which-key-highlight-3-face'
            ("\\`bookmark-" . modi/which-key-highlight-3-face)
            ("\\`counsel-" . modi/which-key-highlight-3-face)
            ;; Highlight using the default `which-key-highlighted-command-face'
            "\\`describe-"
            "\\(rectangle-\\)\\|\\(-rectangle\\)"
            "\\`org-"))

    (which-key-mode 1)))


(provide 'setup-which-key)
