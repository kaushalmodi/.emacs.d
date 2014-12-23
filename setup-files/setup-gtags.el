;; Time-stamp: <2014-12-23 15:32:46 kmodi>

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

    ;; Don't consider ` (back quote) as part of `tag' when looking for a
    ;; Verilog macro definition
    (defun ggtags-tag-at-point ()
      (pcase (funcall ggtags-bounds-of-tag-function)
        (`(,beg . ,end)
         (if (eq ?` (string-to-char (buffer-substring beg end)))
             ;; If `(buffer-substring beg end)' returns "`uvm_info" (for example),
             ;; discard the ` and return just "uvm_info"
             (buffer-substring (1+ beg) end)
           ;; else return the whole `(buffer-substring beg end)'
           (buffer-substring beg end)))))

    (key-chord-define-global "??" 'ggtags-show-definition)))


(provide 'setup-gtags)
