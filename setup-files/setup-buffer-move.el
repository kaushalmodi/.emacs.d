;; Time-stamp: <2015-11-06 11:01:35 kmodi>

;; https://github.com/lukhas/buffer-move

(use-package buffer-move
  :commands (modi/buf-move-left
             modi/buf-move-right
             modi/buf-move-up
             modi/buf-move-down)
  :init
  (progn
    (bind-to-modi-map "," #'modi/buf-move-left)
    (bind-to-modi-map "." #'modi/buf-move-right)
    (bind-to-modi-map "k" #'modi/buf-move-up)
    (bind-to-modi-map "j" #'modi/buf-move-down))
  :config
  (progn
    (setq buffer-move-behavior 'swap) ; values = 'swap (default), 'move

    (defun modi/buf-move-left (swap)
      "Calls `buf-move-left' with the “move” behavior.
If SWAP is non-nil, the default “swap” behavior is used."
      (interactive "P")
      (let ((buffer-move-behavior (if swap 'swap 'move)))
        (buf-move-left)))

    (defun modi/buf-move-right (swap)
      "Calls `buf-move-right' with the “move” behavior.
If SWAP is non-nil, the default “swap” behavior is used."
      (interactive "P")
      (let ((buffer-move-behavior (if swap 'swap 'move)))
        (buf-move-right)))

    (defun modi/buf-move-up (swap)
      "Calls `buf-move-up' with the “move” behavior.
If SWAP is non-nil, the default “swap” behavior is used."
      (interactive "P")
      (let ((buffer-move-behavior (if swap 'swap 'move)))
        (buf-move-up)))

    (defun modi/buf-move-down (swap)
      "Calls `buf-move-down' with the “move” behavior.
If SWAP is non-nil, the default “swap” behavior is used."
      (interactive "P")
      (let ((buffer-move-behavior (if swap 'swap 'move)))
        (buf-move-down)))))


(provide 'setup-buffer-move)
