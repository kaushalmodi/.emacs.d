;; Time-stamp: <2014-08-13 10:46:56 kmodi>

;; header2

(req-package header2
  :commands (auto-make-header)
  :config
  (progn
    (add-hook 'verilog-mode-hook 'auto-make-header)))


(provide 'setup-header2)
