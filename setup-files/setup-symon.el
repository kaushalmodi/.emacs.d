;; Time-stamp: <2015-04-25 01:25:58 kmodi>

;; Mode-line system monitor
;; https://github.com/zk-phi/symon

(use-package symon
  :config
  (progn
    (setq symon-sparkline-type 'symon-sparkline-type-plain)

    (define-symon-monitor symon-current-date-time-monitor
      :interval 60
      :display (propertize
                (format-time-string "%l:%M %b %d %a     ")
                'face 'font-lock-type-face))

    (setq symon-monitors
          (cond ((memq system-type '(gnu/linux cygwin))
                 '(symon-current-date-time-monitor
                   symon-linux-memory-monitor
                   symon-linux-cpu-monitor
                   symon-linux-network-rx-monitor
                   symon-linux-network-tx-monitor))
                ((memq system-type '(windows-nt))
                 '(symon-current-date-time-monitor
                   symon-windows-memory-monitor
                   symon-windows-cpu-monitor
                   symon-windows-network-rx-monitor
                   symon-windows-network-tx-monitor))))

    (symon-mode)))


(provide 'setup-symon)
