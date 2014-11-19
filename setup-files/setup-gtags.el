;; Time-stamp: <2014-11-19 10:19:17 kmodi>

;; gtags, GNU global

(req-package ggtags
  :require (verilog-mode key-chord)
  :init
  (progn
    (setq ggtags-navigation-mode-lighter nil)
    (dolist (hook '(verilog-mode-hook
                    matlab-mode-hook
                    c-mode-hook))
      (add-hook hook 'ggtags-mode))
    (key-chord-define-global "??" 'ggtags-show-definition)))


(provide 'setup-gtags)
