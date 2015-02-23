;; Time-stamp: <2015-02-23 11:42:32 kmodi>

;; Yet Another Folding, Folding code blocks based on indentation.
;; Source: https://github.com/zenozeng/yafolding.el

(use-package yafolding
  :config
  (progn
    (add-hook 'prog-mode-hook 'yafolding-mode)
    (setq yafolding-ellipsis-content ">>>folded")))


(provide 'setup-yafolding)

;; C-Enter   <- Fold the current block; cursor has to be on the top-most line of the block
;; C-S-Enter <- Toggle folding all blocks
