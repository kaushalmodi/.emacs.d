;; Time-stamp: <2015-06-15 17:08:47 kmodi>

(use-package buffer-move
  :config
  (progn
    (bind-to-modi-map "," #'buf-move-left)
    (bind-to-modi-map "." #'buf-move-right)
    (bind-to-modi-map "k" #'buf-move-up)
    (bind-to-modi-map "K" #'buf-move-down)))


(provide 'setup-buffer-move)
