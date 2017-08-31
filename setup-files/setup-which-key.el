;; Time-stamp: <2017-08-31 14:57:19 kmodi>

;; Which Key
;; https://github.com/justbur/emacs-which-key

(use-package which-key
  :defer 10
  :config
  (progn
    (setq which-key-popup-type 'side-window) ;Default
    ;; (setq which-key-popup-type 'minibuffer)

    (setq which-key-compute-remaps t) ;Show correct descriptions for remapped keys

    (setq which-key-allow-multiple-replacements t) ;Default = nil

    (setq which-key-replacement-alist
          '(
            ;; Replacements for how part or whole of FUNCTION is replaced when
            ;; which-key displays
            ;;   KEY → FUNCTION
            ;; Eg: After "d" in `calc', display "6 → calc-hex-radix" as "6 → 🖩hex-radix"
            ((nil . "Prefix Command")           . (nil . "prefix"))
            ((nil . "which-key-show-next-page") . (nil . "wk next pg"))
            ((nil . "\\`calc-")                  . (nil . "")) ;Hide "calc-" prefixes when listing M-x calc keys
            ((nil . "\\`artist-select-op-")      . (nil . "")) ;Make artist-mode function names less verbose
            ((nil . "\\`artist-select-")         . (nil . "sel-"))
            ((nil . "\\`artist-toggle-")         . (nil . "toggle-"))
            ((nil . "modi/")                    . (nil . "m/")) ;The car is intentionally not "\\`modi/" to cover cases like `hydra-toggle/modi/..'.
            ((nil . "\\`hydra-\\(.+\\)/body\\'")      . (nil . "h/\\1"))
            ((nil . "\\`org-babel-")             . (nil . "ob/"))
            ;; Replacements for how KEY is replaced when which-key displays
            ;;   KEY → FUNCTION
            ;; Eg: After "C-c", display "right → winner-redo" as "⇨ → winner-redo"
            (("<\\(.*\\)-?left>"   . nil)         . ("\\1⇦" . nil))
            (("<\\(.*\\)-?right>"  . nil)         . ("\\1⇨" . nil))
            (("<\\(.*\\)-?up>"     . nil)         . ("\\1⇧" . nil))
            (("<\\(.*\\)-?down>"   . nil)         . ("\\1⇩" . nil))
            (("<\\(.*\\)-?return>" . nil)         . ("\\1⏎" . nil))
            (("RET" . nil)                      . ("⏎" . nil))
            (("<\\(.*\\)-?delete>" . nil)         . ("\\1⮽" . nil)) ;Delete key
            (("DEL"  . nil)                     . ("BS" . nil)) ;Backspace key
            (("<\\(.*\\)-?backspace>" . nil)      . ("\\1BS" . nil)) ;Backspace key
            (("<\\(.*\\)-?tab>"   . nil)          . ("\\1TAB" . nil))
            (("SPC"   . nil)                    . ("⼐" . nil))
            (("<\\(.*\\)-?next>"   . nil)         . ("\\1PgDn" . nil))
            (("<\\(.*\\)-?prior>"  . nil)         . ("\\1PgUp" . nil))
            ))
    ;; Use cool unicode characters if available
    (with-eval-after-load 'setup-font-check
      (when font-symbola-p
        (add-to-list 'which-key-replacement-alist '((nil . "\\`calc-") . (nil . "🖩")))
        (add-to-list 'which-key-replacement-alist '((nil . "\\`engine/search-") . (nil . "🔎 "))))) ;engine-mode

    ;; Change what string to display for a given *complete* key binding
    ;; Eg: After "C-x", display "8 → +unicode" instead of "8 → +prefix"
    (which-key-add-key-based-replacements
      "C-x 8"   "unicode"
      "C-x a"   "abbrev/expand"
      "C-x r"   "rectangle/register/bookmark"
      "C-x v"   "version control"
      "C-c /"   "engine-mode-map"
      "C-c C-v" "org-babel"
      "C-x 8 0" "ZWS")

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
