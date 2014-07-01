;; Time-stamp: <2014-07-01 12:20:25 kmodi>

;; NLinum

(require 'nlinum)

(setq global-nlinum-mode -1
      nlinum-format      "%4d ") ; right aligned, 4 char wide line num col

;; NOTE: nlinum mode is turned on in the setup files of the desired major modes


(setq setup-linum-loaded t)
(provide 'setup-linum)
