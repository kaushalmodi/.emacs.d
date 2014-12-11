;; Time-stamp: <2014-12-11 14:03:10 kmodi>

;; gtags, GNU global

(req-package ggtags
  :require (verilog-mode key-chord)
  :init
  (progn
    (setq ggtags-navigation-mode-lighter nil)
    (setq ggtags-oversize-limit (* 30 1024 1024))

    (dolist (hook '(verilog-mode-hook
                    matlab-mode-hook
                    c-mode-hook))
      (add-hook hook 'ggtags-mode))

    (key-chord-define-global "??" 'ggtags-show-definition)))


(provide 'setup-gtags)
