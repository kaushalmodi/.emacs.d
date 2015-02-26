;; Time-stamp: <2015-02-26 08:18:21 kmodi>

;; Yet Another Folding, Folding code blocks based on indentation.
;; Source: https://github.com/zenozeng/yafolding.el

(use-package yafolding
    :config
  (progn
    (setq yafolding-ellipsis-content ">>>folded")

    ;; faces
    (set-face-attribute 'yafolding-ellipsis-face nil
                        :foreground "deep sky blue"
                        :slant      'italic
                        :weight     'bold
                        :height     1.1)

    (add-hook 'prog-mode-hook #'yafolding-mode)))


(provide 'setup-yafolding)

;; C-Enter   <- Fold the current block; cursor has to be on the top-most line of the block
;; C-S-Enter <- Toggle folding all blocks
