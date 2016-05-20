;; Time-stamp: <2016-05-20 00:13:41 kmodi>

;; https://github.com/lukhas/buffer-move

(use-package buffer-move
  :defer t
  :init
  (progn
    (bind-to-modi-map "," #'buf-move-left)
    (bind-to-modi-map "." #'buf-move-right)
    (bind-to-modi-map "k" #'buf-move-up)
    (bind-to-modi-map "j" #'buf-move-down))
  :config
  (progn
    (defun modi/buf-move-swap-option (orig-fun &rest args)
      "Set default `buf-move-to' behavior to “move” buffers.
If prefix argument is used, use the “swap” behavior."
      (let ((buffer-move-behavior (if current-prefix-arg 'swap 'move)))
        (apply orig-fun args)))
    (advice-add 'buf-move-to :around #'modi/buf-move-swap-option)))


(provide 'setup-buffer-move)
