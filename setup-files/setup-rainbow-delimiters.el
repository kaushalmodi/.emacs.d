;; Time-stamp: <2014-08-13 09:35:52 kmodi>

;; Rainbow Delimiters
;; http://www.emacswiki.org/emacs/RainbowDelimiters

(req-package rainbow-delimiters
  :config
  (progn
    ;; Enable in all programming-related modes
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))


(provide 'setup-rainbow-delimiters)
