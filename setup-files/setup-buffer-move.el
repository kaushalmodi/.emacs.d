;; Time-stamp: <2014-08-19 09:45:52 kmodi>

(req-package buffer-move
  :config
  (progn
    (bind-to-modi-map "," buf-move-left)
    (bind-to-modi-map "." buf-move-right)
    (bind-to-modi-map "k" buf-move-up)
    (bind-to-modi-map "K" buf-move-down)))


(provide 'setup-buffer-move)
