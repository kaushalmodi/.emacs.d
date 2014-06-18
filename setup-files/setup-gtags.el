;; Time-stamp: <2014-06-12 18:24:48 kmodi>

;; gtags, GNU global

(require 'ggtags)

;; Turn on ggtags-mode automatically for the following mode hooks
(add-hook 'verilog-mode-hook
          (lambda()
            (ggtags-mode 1)))
(add-hook 'emacs-lisp-mode-hook
          (lambda()
            (ggtags-mode 1)))
(add-hook 'matlab-mode-hook
          (lambda()
            (ggtags-mode 1)))


(setq setup-gtags-loaded t)
(provide 'setup-gtags)
