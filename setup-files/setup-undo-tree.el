;; Time-stamp: <2014-03-13 11:04:59 kmodi>

;; Undo Tree
;; http://www.dr-qubit.org/emacs.php

(require 'undo-tree)

(global-undo-tree-mode)

;; customize undo-tree-map
(define-key undo-tree-map (kbd "C-/")       'undo-tree-undo)
(define-key undo-tree-map (kbd "C-_")       'undo-tree-undo)
;; (define-key undo-tree-map (kbd "M-_")       'undo-tree-redo)
;; (define-key undo-tree-map (kbd "C-?")       'undo-tree-redo)
(define-key undo-tree-map (kbd "C-?")       nil)
;; (define-key undo-tree-map (kbd "\C-x u")    'undo-tree-visualize)

;; customize undo-tree-visualizer-mode-map bindings
(define-key undo-tree-visualizer-mode-map "k" 'undo-tree-visualize-undo)
(define-key undo-tree-visualizer-mode-map "j" 'undo-tree-visualize-redo)
(define-key undo-tree-visualizer-mode-map "h" 'undo-tree-visualize-switch-branch-left)
(define-key undo-tree-visualizer-mode-map "l" 'undo-tree-visualize-switch-branch-right)
;; (define-key undo-tree-visualizer-mode-map "\M-{" 'undo-tree-visualize-undo-to-x)
;; (define-key undo-tree-visualizer-mode-map "\M-}" 'undo-tree-visualize-redo-to-x)
;; (define-key undo-tree-visualizer-mode-map [C-up] 'undo-tree-visualize-undo-to-x)
;; (define-key undo-tree-visualizer-mode-map [C-down] 'undo-tree-visualize-redo-to-x)
;; ;; mouse sets buffer state to node at click
;; (define-key undo-tree-visualizer-mode-map [mouse-1] 'undo-tree-visualizer-mouse-set)
;; ;; toggle timestamps
;; (define-key undo-tree-visualizer-mode-map "t" 'undo-tree-visualizer-toggle-timestamps)
;; ;; toggle diff
;; (define-key undo-tree-visualizer-mode-map "d" 'undo-tree-visualizer-toggle-diff)
;; ;; toggle selection mode
;; (define-key undo-tree-visualizer-mode-map "s" 'undo-tree-visualizer-selection-mode)
;; ;; horizontal scrolling may be needed if the tree is very wide
;; (define-key undo-tree-visualizer-mode-map "," 'undo-tree-visualizer-scroll-left)
;; (define-key undo-tree-visualizer-mode-map "." 'undo-tree-visualizer-scroll-right)
;; (define-key undo-tree-visualizer-mode-map "<" 'undo-tree-visualizer-scroll-left)
;; (define-key undo-tree-visualizer-mode-map ">" 'undo-tree-visualizer-scroll-right)
;; ;; vertical scrolling may be needed if the tree is very tall
;; (define-key undo-tree-visualizer-mode-map [next] 'undo-tree-visualizer-scroll-up)
;; (define-key undo-tree-visualizer-mode-map [prior] 'undo-tree-visualizer-scroll-down)
;; ;; quit/abort visualizer
;; (define-key undo-tree-visualizer-mode-map "q" 'undo-tree-visualizer-quit)


(setq setup-undo-tree-loaded t)
(provide 'setup-undo-tree)
