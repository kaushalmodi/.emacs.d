;; Time-stamp: <2014-10-21 09:38:32 kmodi>

;; Guide Key
;; https://github.com/kai2nenobu/guide-key

(req-package guide-key
  :config
  (progn
    (setq guide-key/guide-key-sequence '("C-x r"   ; rectangle, registers
                                         "C-x 4"   ; commands that operate in other window (buffer)
                                         "C-x 5"   ; commands that operate in other frame (window)
                                         "C-c p"   ; projectile
                                         "C-x m"   ; my custom pseudo map
                                         "C-x"
                                         "C-c"
                                         "C-c C-c"
                                         "C-c C-t" ; verilog-mode insert blocks
                                         "s-f"     ; fiplr
                                         "s-w"     ; workgroups2
                                         "C-c p"   ; projectile
                                         "C-c p 4" ; projectile
                                         )
          guide-key/highlight-command-regexp "rectangle"
          ;; guide-key can highlight commands which match a specified regular expression.
          ;; Key bindings following "C-x r" are rectangle family and register family.
          ;; Below setting highlights only rectangle family commands.
          guide-key/idle-delay 1 ;; delay before the guide shows up, default is 1 second
          guide-key/popup-window-position 'bottom) ;; show guide key popup at bottom
    (guide-key-mode 1)))  ; Enable guide-key-mode


(provide 'setup-guide-key)
