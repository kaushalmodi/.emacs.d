;; Time-stamp: <2014-08-19 12:15:30 kmodi>

;; gtags, GNU global

(req-package ggtags
  :require (verilog-mode key-chord)
  :init
  (progn
    (dolist (hook '(verilog-mode-hook
                    matlab-mode-hook))
      (add-hook hook 'ggtags-mode))
    (key-chord-define-global "??" 'ggtags-show-definition)))


(provide 'setup-gtags)
