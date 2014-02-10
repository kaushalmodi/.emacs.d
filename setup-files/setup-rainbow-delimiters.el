;; Time-stamp: <2014-02-10 17:44:19 kmodi>

;; Rainbow Delimiters
;; http://www.emacswiki.org/emacs/RainbowDelimiters

(require 'rainbow-delimiters)

;; Enable in all programming-related modes
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(setq setup-rainbow-delimiters-loaded t)
(provide 'setup-rainbow-delimiters)
