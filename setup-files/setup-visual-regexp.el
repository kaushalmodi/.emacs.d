;; Time-stamp: <2014-08-13 11:34:01 kmodi>

;; Visual Regular Expression search/replace

(req-package visual-regexp
  :require (region-bindings-mode)
  :config
  (progn
    (bind-keys
     :map modi-mode-map
     ("C-M-%" . vr/query-replace) ;; replace the emacs default query-replace-regexp
     ("C-c q" . vr/query-replace))

    (bind-keys
     :map region-bindings-mode-map
     ("}" . vr/query-replace))))


(provide 'setup-visual-regexp)
