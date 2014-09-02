;; Time-stamp: <2014-08-12 17:39:59 kmodi>

;; Highlight Symbol at point/cursor
;; (require 'highlight-symbol)
;; NOTE: highlight-symbol package is no longer required. The highlight-global
;; package does a very good job!

;; Highlight Global
;; Source: https://github.com/glen-dai/highlight-global
;; Unlike highlight-symbol which only do highlighting in current buffer, this
;; package highlights all matches accross ALL buffers. Multiple highlights are
;; supported. Different highlight shows in different faces.
;; There are 2 ways to select a highlight target: 1. Mark the selection by
;; region (very useful when you want to highlight a pattern accross all symbols),
;; 2. Put cursor on a symbol to pick the symbol to highlight.
(req-package highlight-global
  :config
  (progn
    (bind-to-modi-map "h" highlight-frame-toggle)
    (bind-to-modi-map "H" clear-highlight-frame)))


;; Volatile Highlights
;; https://github.com/k-talo/volatile-highlights.el
(req-package volatile-highlights
  :config
  (progn
    (volatile-highlights-mode t)))


;; Auto Highlight Symbol
;; https://github.com/emacsmirror/auto-highlight-symbol
(req-package auto-highlight-symbol
  :config
  (progn
    (bind-keys
     :map modi-mode-map
     ("C-*"             . auto-highlight-symbol-mode)
     ("<C-kp-multiply>" . auto-highlight-symbol-mode))
    (bind-keys
     :map auto-highlight-symbol-mode-map
     ("M-<left>"    . ahs-backward)
     ("M-<right>"   . ahs-forward)
     ("M-S-<left>"  . ahs-backward-definition)
     ("M-S-<right>" . ahs-forward-definition)
     ("M--"         . ahs-back-to-start)
     ("C-x C-'"     . ahs-change-range)
     ("C-x C-a"     . ahs-edit-mode))))


(provide 'setup-highlight)
