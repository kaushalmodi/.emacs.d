;; Time-stamp: <2014-01-24 12:46:37 kmodi>

;; Guide Key
;; Source: https://github.com/kbkbkbkb1/guide-key

(require 'guide-key)

(setq guide-key/guide-key-sequence '("C-x r" ;; rectangle, registers
                                     "C-x 4" ;; commands that operate in other window (buffer)
                                     "C-x 5" ;; commands that operate in other frame (window)
                                     "C-c p" ;; projectile
                                     "C-x m" ;; my custom map: modi-map
                                     "C-x"
                                     "C-c"
                                     )
      guide-key/highlight-command-regexp "rectangle"
      ;; guide-key can highlight commands which match a specified regular expression.
      ;; Key bindings following "C-x r" are rectangle family and register family.
      ;; Below setting highlights only rectangle family commands.
      guide-key/idle-delay 1 ;; delay before the guide shows up, default is 1 second
      guide-key/popup-window-position 'bottom ;; show guide key popup at bottom
      )
(guide-key-mode 1)  ; Enable guide-key-mode

(provide 'setup-guide-key)
