;; Time-stamp: <2014-01-13 15:51:02 kmodi>

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


(setq setup-highlight-loaded t)
(provide 'setup-highlight)
