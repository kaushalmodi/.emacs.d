;; Time-stamp: <2015-06-08 17:23:23 kmodi>

;; Fold This
;; https://github.com/magnars/fold-this.el

(use-package fold-this
  :config
  (progn
    (setq fold-this-persistent-folds-file (locate-user-emacs-file "saved-folds"))
    (setq fold-this-persistent-folds t)

    (bind-keys
     :map fold-this-keymap
      ("<mouse-1>" . fold-this-unfold-at-point)) ; left-click on ellipsis to unfold
    (when (featurep 'region-bindings-mode)
      (bind-keys
       :map region-bindings-mode-map
        ("f" . fold-this)))
    ;; Below bindings are made in global map and not in my minor mode as I want
    ;; other modes to override those bindings
    (bind-keys
     ("C-c C-f" . fold-this))

    (defun modi/unfold-if-last-command-fold (&rest args)
      "If the `last-command' was `fold-this', undo that fold action."
      (let ((last-cmd-was-fold (eq last-command #'fold-this)))
        (when last-cmd-was-fold
          (backward-char 1)
          (fold-this-unfold-at-point))
        last-cmd-was-fold))
    ;; Advice `undo-tree-undo' to unfold the previous fold
    (with-eval-after-load 'undo-tree
      (advice-add #'undo-tree-undo :before-until #'modi/unfold-if-last-command-fold))
    ;; Advice `undo' to unfold the previous fold
    (with-eval-after-load 'undo-tree
      (advice-add #'undo :before-until #'modi/unfold-if-last-command-fold))))


(provide 'setup-fold-this)

;; Fold the selected region using `fold-this' binding.
;; If you move point into the ellipsis and press `RET' or `C-g', it is unfolded.
