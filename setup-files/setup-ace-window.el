;; Time-stamp: <2014-08-13 10:24:34 kmodi>

;; Ace Window
(req-package ace-window
  :config
  (progn
    (bind-keys
     :map modi-mode-map
     ;; Important to use my minor mode map as I want my bindings to override
     ;; bindings in other major modes (esp org-mode)
     ("C-x o" . ace-window))))


(provide 'setup-ace-window)
