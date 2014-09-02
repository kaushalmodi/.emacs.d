;; Time-stamp: <2014-08-13 12:52:55 kmodi>

;; Popwin
;; Source: https://github.com/m2ym/popwin-el
;; popwin is a popup window manager for Emacs which makes you free from the hell
;; of annoying buffers such like *Help*, *Completions*, *compilation*, and etc.
;; Windows of such temporary buffers will be shown as a popup window, and you
;; can close them smoothly by typing `C-g' in anytime.

(req-package popwin
  :init
  (progn
    (popwin-mode 1)))


(provide 'setup-popwin)
