;; Time-stamp: <2015-02-23 11:42:32 kmodi>

;; Rainbow Delimiters
;; http://www.emacswiki.org/emacs/RainbowDelimiters

(use-package rainbow-delimiters
  :config
  (progn
    ;; Enable in all programming-related modes
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)))


(provide 'setup-rainbow-delimiters)
