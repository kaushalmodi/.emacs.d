;; Time-stamp: <2014-07-24 22:04:27 kmodi>

;; Yet Another Folding, Folding code blocks based on indentation.
;; Source: https://github.com/zenozeng/yafolding.el

(require 'yafolding)

(add-hook 'prog-mode-hook
          (lambda () (yafolding-mode)))

(setq yafolding-ellipsis-content ">>>folded")


(setq setup-yafolding-loaded t)
(provide 'setup-yafolding)

;; C-Enter <- Fold the current block; cursor has to be on the top-most line of the block
;; C-S-Enter <- Toggle folding all blocks
