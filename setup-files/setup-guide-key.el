;; Time-stamp: <2015-02-20 11:22:47 kmodi>

;; Guide Key
;; https://github.com/kai2nenobu/guide-key

(req-package guide-key
  :config
  (progn
    (setq guide-key/recursive-key-sequence-flag t)
    (setq guide-key/guide-key-sequence '(
                                         "C-x"
                                         ;; "C-x r"   ; rectangle, registers
                                         ;; "C-x 4"   ; commands that operate in other window (buffer)
                                         ;; "C-x 5"   ; commands that operate in other frame (window)
                                         ;; "C-x m"   ; my custom pseudo map
                                         "C-c"
                                         ;; "C-c C-c"
                                         ;; "C-c C-t" ; verilog-mode insert blocks
                                         ;; "C-c p"   ; projectile
                                         ;; "C-c p 4" ; projectile
                                         "M-#" ; outshine
                                         "C-h"
                                         ))
    (setq guide-key/highlight-command-regexp "rectangle")
    ;; guide-key can highlight commands which match a specified regular expression.
    ;; Key bindings following "C-x r" are rectangle family and register family.
    ;; Below setting highlights only rectangle family commands.
    (setq guide-key/idle-delay 1) ; delay before the guide shows up, default is 1 second
    (setq guide-key/popup-window-position 'bottom) ; show guide key popup at bottom
    (guide-key-mode 1))) ; Enable guide-key-mode


(provide 'setup-guide-key)
