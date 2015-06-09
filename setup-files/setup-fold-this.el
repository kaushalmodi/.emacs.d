;; Time-stamp: <2015-06-09 00:02:38 kmodi>

;; Fold This
;; https://github.com/magnars/fold-this.el

(use-package fold-this
  :config
  (progn
    (setq fold-this-persistent-folds-file (locate-user-emacs-file "saved-folds"))
    (setq fold-this-persistent-folds t)

    (defvar modi/fold-this--last-overlay nil
      "Store the last overlay created by `fold-this'.")

    (defun fold-this (beg end)
      (interactive "r")
      (let ((o (make-overlay beg end nil t nil)))
        (overlay-put o 'type 'fold-this)
        (overlay-put o 'invisible t)
        (overlay-put o 'keymap fold-this-keymap)
        (overlay-put o 'face 'fold-this-overlay)
        (overlay-put o 'modification-hooks '(fold-this--unfold-overlay))
        (overlay-put o 'display (propertize "..." 'face 'fold-this-overlay))
        (overlay-put o 'evaporate t)
        (setq modi/fold-this--last-overlay o))
      (deactivate-mark))

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
          (delete-overlay modi/fold-this--last-overlay))
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
