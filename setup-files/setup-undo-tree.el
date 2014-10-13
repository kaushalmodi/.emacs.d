;; Time-stamp: <2014-10-01 16:44:44 kmodi>

;; Undo Tree
;; http://www.dr-qubit.org/emacs.php

(req-package undo-tree
  :require (key-chord)
  :config
  (progn
    (global-undo-tree-mode)
    (bind-keys
     :map undo-tree-map
     ("C-/"    . undo-tree-undo)
     ("C-_"    . undo-tree-undo)
     ("M-_"    . undo-tree-redo) ;; default
     ("\C-x u" . undo-tree-visualize)) ;; default
    (define-key undo-tree-map (kbd "C-?") nil)
    (bind-keys
     :map undo-tree-visualizer-mode-map
     ;; customize undo-tree-visualizer-mode-map bindings
     ("k"         . undo-tree-visualize-undo)
     ("j"         . undo-tree-visualize-redo)
     ("h"         . undo-tree-visualize-switch-branch-left)
     ("l"         . undo-tree-visualize-switch-branch-right)
     ("\M-{"      . undo-tree-visualize-undo-to-x) ;; default
     ("\M-}"      . undo-tree-visualize-redo-to-x) ;; default
     ("<C-up>"    . undo-tree-visualize-undo-to-x) ;; default
     ("<C-down>"  . undo-tree-visualize-redo-to-x) ;; default
     ;; mouse sets buffer state to node at click
     ("<mouse-1>" . undo-tree-visualizer-mouse-set) ;; default
     ("t"         . undo-tree-visualizer-toggle-timestamps) ;; default
     ("d"         . undo-tree-visualizer-toggle-diff) ;; default
     ("s"         . undo-tree-visualizer-selection-mode) ;; default
     ;; horizontal scrolling may be needed if the tree is very wide
     (","         . undo-tree-visualizer-scroll-left) ;; default
     ("."         . undo-tree-visualizer-scroll-right) ;; default
     ("<"         . undo-tree-visualizer-scroll-left) ;; default
     (">"         . undo-tree-visualizer-scroll-right) ;; default
     ;; vertical scrolling may be needed if the tree is very tall
     ("<next>"    . undo-tree-visualizer-scroll-up) ;; default
     ("<prior>"   . undo-tree-visualizer-scroll-down) ;; default
     ;; quit/abort visualizer
     ("q"         . undo-tree-visualizer-quit)) ;; default
    (key-chord-define-global "UU"   'undo-tree-redo)))


(provide 'setup-undo-tree)
