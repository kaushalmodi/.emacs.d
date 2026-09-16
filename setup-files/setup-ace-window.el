;; -*- lexical-binding: t; -*-
;; Time-stamp: <2016-05-07 01:13:15 kmodi>

;; Ace Window
;; https://github.com/abo-abo/ace-window

(use-package ace-window
  :bind  (:map modi-mode-map
          ("C-x o" . ace-window))
  :init
  (progn
    ;; Show the window key in the mode line from the start. Enabling this
    ;; from `:config' would not do anything until the first `C-x o' loaded
    ;; ace-window, which is exactly when the key is needed.
    (ace-window-display-mode 1))
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
           'face 'modi/aw-mode-line-face)))))))


(provide 'setup-ace-window)

;;         `ace-window-BINDING' -> `ace-select-window'
;;     C-u `ace-window-BINDING' -> `ace-swap-window'
;; C-u C-u `ace-window-BINDING' -> `ace-delete-window'

;; ace-window dispatch operations:
;;
;; | x | aw-delete-window     | Ace - Delete Window     |
;; | m | aw-swap-window       | Ace - Swap Window       |
;; | n | aw-flip-window       |                         |
;; | v | aw-split-window-vert | Ace - Split Vert Window |
;; | b | aw-split-window-horz | Ace - Split Horz Window |
;; | i | delete-other-windows | Ace - Maximize Window   |
;; | o | delete-other-windows |                         |
;;
;; Usage: <ace-window BINDING> x <WINDOW-CHAR> will delete that window
