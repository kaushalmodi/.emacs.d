;; Time-stamp: <2013-12-06 14:25:21 kmodi>

;; Popwin
;; Source: https://github.com/m2ym/popwin-el
;; popwin is a popup window manager for Emacs which makes you free from the hell
;; of annoying buffers such like *Help*, *Completions*, *compilation*, and etc.
;; Windows of such temporary buffers will be shown as a popup window, and you
;; can close them smoothly by typing `C-g' in anytime.

(require 'popwin)

(popwin-mode 1)

(setq setup-popwin-loaded t)
(provide 'setup-popwin)
