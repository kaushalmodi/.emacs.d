;; Time-stamp: <2014-04-01 10:39:40 kmodi>

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
(require 'highlight-global)

;; Volatile Highlights
;; https://github.com/k-talo/volatile-highlights.el
(require 'volatile-highlights)
(volatile-highlights-mode t)

;; Auto Highlight Symbol
;; https://github.com/emacsmirror/auto-highlight-symbol
(require 'auto-highlight-symbol)
;; ahs mode key bindings
(define-key auto-highlight-symbol-mode-map (kbd "M-<left>"    ) 'ahs-backward            )
(define-key auto-highlight-symbol-mode-map (kbd "M-<right>"   ) 'ahs-forward             )
(define-key auto-highlight-symbol-mode-map (kbd "M-S-<left>"  ) 'ahs-backward-definition )
(define-key auto-highlight-symbol-mode-map (kbd "M-S-<right>" ) 'ahs-forward-definition  )
(define-key auto-highlight-symbol-mode-map (kbd "M--"         ) 'ahs-back-to-start       )
(define-key auto-highlight-symbol-mode-map (kbd "C-x C-'"     ) 'ahs-change-range        )
(define-key auto-highlight-symbol-mode-map (kbd "C-x C-a"     ) 'ahs-edit-mode           )


(setq setup-highlight-loaded t)
(provide 'setup-highlight)
