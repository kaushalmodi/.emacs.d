;; Time-stamp: <2015-02-23 11:42:32 kmodi>

;; Ace Window
;; Source: https://github.com/abo-abo/ace-window

(use-package ace-window
  :config
  (progn
    (bind-keys
     :map modi-mode-map
     ;; Important to use my minor mode map as I want my bindings to override
     ;; bindings in other major modes (esp org-mode)
     ("C-x o" . ace-window))))


(provide 'setup-ace-window)

;;         `ace-window-BINDING' -> `ace-select-window'
;;     C-u `ace-window-BINDING' -> `ace-swap-window'
;; C-u C-u `ace-window-BINDING' -> `ace-delete-window'
