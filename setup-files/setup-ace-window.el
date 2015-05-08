;; Time-stamp: <2015-05-08 10:57:12 kmodi>

;; Ace Window
;; https://github.com/abo-abo/ace-window

(use-package ace-window
  :config
  (progn

    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

    (defface modi/aw-mode-line-face
      '((((background dark)) (:foreground "gray50"))
        (t                   (:foreground "black")))
      "Face used for displaying the ace window key in the mode-line.")

    ;; Patch `aw-update' to use the background sensitive face
    (defun aw-update ()
      "Update ace-window-path window parameter for all windows."
      (avy-traverse
       (avy-tree (aw-window-list) aw-keys)
       (lambda (path leaf)
         (set-window-parameter
          leaf 'ace-window-path
          (propertize
           (apply #'string (reverse path))
           'face 'modi/aw-mode-line-face)))))

    (bind-keys
     :map modi-mode-map
      ;; Important to use my minor mode map as I want my bindings to override
      ;; bindings in other major modes (esp org-mode)
      ("C-x o" . ace-window)
      ("M-o"   . ace-window))

    (ace-window-display-mode 1)))


(provide 'setup-ace-window)

;;         `ace-window-BINDING' -> `ace-select-window'
;;     C-u `ace-window-BINDING' -> `ace-swap-window'
;; C-u C-u `ace-window-BINDING' -> `ace-delete-window'
