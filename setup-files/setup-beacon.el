;; Time-stamp: <2015-10-18 11:41:54 kmodi>

;; Beacon
;; https://github.com/Malabarba/beacon

(use-package beacon
  :config
  (progn
    (setq beacon-blink-when-point-moves nil) ; default nil
    (setq beacon-blink-when-buffer-changes t) ; default t
    (setq beacon-blink-when-window-scrolls t) ; default t
    (setq beacon-blink-when-window-changes t) ; default t

    (setq beacon-blink-duration 0.3) ; default 0.3
    (setq beacon-blink-delay 0.3) ; default 0.3
    (setq beacon-size 20) ; default 40
    ;; (setq beacon-color "yellow") ; default 0.5
    (setq beacon-color 0.5) ; default 0.5

    (beacon-mode 1)))


(provide 'setup-beacon)
