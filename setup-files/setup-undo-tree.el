;; Time-stamp: <2015-03-20 13:51:15 kmodi>

;; Undo Tree
;; http://www.dr-qubit.org/emacs.php

(use-package undo-tree
    :config
  (progn
    (bind-keys
     :map undo-tree-map
     ("C-_" . undo-tree-undo)
     ("C-/" . undo-tree-undo)
     ("M-_" . undo-tree-redo)) ; default

    (global-undo-tree-mode)))


(provide 'setup-undo-tree)

;; `undo-tree' package also remaps `undo' and `undo-only' to `undo-tree-undo'

;; * `undo-tree-visualize' bindings
;;
;; | [remap previous-line] | 'undo-tree-visualize-undo |
;; | [remap next-line]     | 'undo-tree-visualize-redo |
;; | [up]                  | 'undo-tree-visualize-undo |
;; | "p"                   | 'undo-tree-visualize-undo |
;; | "\C-p"                | 'undo-tree-visualize-undo |
;; | [down]                | 'undo-tree-visualize-redo |
;; | "n"                   | 'undo-tree-visualize-redo |
;; | "\C-n"                | 'undo-tree-visualize-redo |
;;
;; Horizontal motion keys switch branch
;; | [remap forward-char]  | 'undo-tree-visualize-switch-branch-right) |
;; | [remap backward-char] | 'undo-tree-visualize-switch-branch-left)  |
;; | [right]               | 'undo-tree-visualize-switch-branch-right  |
;; | "f"                   | 'undo-tree-visualize-switch-branch-right  |
;; | "\C-f"                | 'undo-tree-visualize-switch-branch-right  |
;; | [left]                | 'undo-tree-visualize-switch-branch-left   |
;; | "b"                   | 'undo-tree-visualize-switch-branch-left   |
;; | "\C-b"                | 'undo-tree-visualize-switch-branch-left   |
;;
;; Paragraph motion keys undo/redo to significant points in tree
;; | [remap backward-paragraph] | 'undo-tree-visualize-undo-to-x |
;; | [remap forward-paragraph]  | 'undo-tree-visualize-redo-to-x |
;; | "\M-{"                     | 'undo-tree-visualize-undo-to-x |
;; | "\M-}"                     | 'undo-tree-visualize-redo-to-x |
;; | [C-up]                     | 'undo-tree-visualize-undo-to-x |
;; | [C-down]                   | 'undo-tree-visualize-redo-to-x |
;;
;; Mouse sets buffer state to node at click
;; | [mouse-1] | 'undo-tree-visualizer-mouse-set |
;;
;; Toggle timestamps
;; | "t" | 'undo-tree-visualizer-toggle-timestamps |
;;
;; Toggle diff
;; | "d" | 'undo-tree-visualizer-toggle-diff |
;;
;; Toggle selection mode
;; | "s" | 'undo-tree-visualizer-selection-mode |
;;
;; Horizontal scrolling may be needed if the tree is very wide
;; | "," | 'undo-tree-visualizer-scroll-left  |
;; | "." | 'undo-tree-visualizer-scroll-right |
;; | "<" | 'undo-tree-visualizer-scroll-left  |
;; | ">" | 'undo-tree-visualizer-scroll-right |
;;
;; Vertical scrolling may be needed if the tree is very tall
;; | [next]  | 'undo-tree-visualizer-scroll-up   |
;; | [prior] | 'undo-tree-visualizer-scroll-down |
;;
;; Quit/abort visualizer
;; | "q"    | 'undo-tree-visualizer-quit  |
;; | "\C-q" | 'undo-tree-visualizer-abort |
