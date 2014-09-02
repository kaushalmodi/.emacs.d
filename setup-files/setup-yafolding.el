;; Time-stamp: <2014-08-13 15:05:20 kmodi>

;; Yet Another Folding, Folding code blocks based on indentation.
;; Source: https://github.com/zenozeng/yafolding.el

(req-package yafolding
  :config
  (progn
    (add-hook 'prog-mode-hook 'yafolding-mode)
    (setq yafolding-ellipsis-content ">>>folded")))


(provide 'setup-yafolding)

;; C-Enter   <- Fold the current block; cursor has to be on the top-most line of the block
;; C-S-Enter <- Toggle folding all blocks
