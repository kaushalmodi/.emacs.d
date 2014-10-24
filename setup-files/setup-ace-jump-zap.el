;; Time-stamp: <2014-10-24 15:06:21 kmodi>

;; Ace Jump Zap

(req-package ace-jump-zap
  :require key-chord
  :init
  (progn
    (bind-to-modi-map "z" ace-jump-zap-up-to-char-dwim)
    (bind-to-modi-map "Z" ace-jump-zap-up-to-char)))


(provide 'setup-ace-jump-zap)

;;     `ace-jump-zap-up-to-char-dwim-BINDING' -> `zap-up-to-char'
;; C-u `ace-jump-zap-up-to-char-dwim-BINDING' -> `ace-jump-zap-up-to-char'
