;; Time-stamp: <2014-06-19 09:23:25 kmodi>

;; gtags, GNU global

(require 'ggtags)

;; Turn on ggtags-mode automatically for the following mode hooks
(add-hook 'verilog-mode-hook
          (lambda()
            (ggtags-mode 1)))
(add-hook 'matlab-mode-hook
          (lambda()
            (ggtags-mode 1)))


(setq setup-gtags-loaded t)
(provide 'setup-gtags)
