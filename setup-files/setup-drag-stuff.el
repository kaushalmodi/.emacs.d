;; Time-stamp: <2015-07-24 15:30:07 kmodi>

;; Drag Stuff
;; https://github.com/rejeep/drag-stuff

(use-package drag-stuff
  :config
  (progn
    ;; Do not use `drag-stuff' keys
    (defun drag-stuff-define-keys ())

    ;; http://emacs.stackexchange.com/a/13942/115
    ;; https://github.com/rejeep/drag-stuff.el/issues/4
    (defvar modi/drag-stuff--point-adjusted nil)
    (defvar modi/drag-stuff--point-mark-exchanged nil)

    (defun modi/drag-stuff--adj-pt-pre-drag ()
      "If a region is selected AND the `point' is in the first column, move
back the point by one char so that it ends up on the previous line. If the
point is above the mark, exchange the point and mark temporarily."
      (when (region-active-p)
        (when (< (point) (mark)) ; selection is done starting from bottom to up
          (exchange-point-and-mark)
          (setq modi/drag-stuff--point-mark-exchanged t))
        (if (zerop (current-column))
            (progn
              (backward-char 1)
              (setq modi/drag-stuff--point-adjusted t))
          ;; If point did not end up being on the first column after the
          ;; point/mark exchange, revert that exchange.
          (when modi/drag-stuff--point-mark-exchanged
            (exchange-point-and-mark) ; restore the original point and mark loc
            (setq modi/drag-stuff--point-mark-exchanged nil)))))

    (defun modi/drag-stuff--rst-pt-post-drag ()
      "Restore the `point' to where it was by forwarding it by one char after
the vertical drag is done."
      (when modi/drag-stuff--point-adjusted
        (forward-char 1)
        (setq modi/drag-stuff--point-adjusted nil))
      (when modi/drag-stuff--point-mark-exchanged
        (exchange-point-and-mark) ; restore the original point and mark loc
        (setq modi/drag-stuff--point-mark-exchanged nil)))

    (add-hook 'drag-stuff-before-drag-hook #'modi/drag-stuff--adj-pt-pre-drag)
    (add-hook 'drag-stuff-after-drag-hook  #'modi/drag-stuff--rst-pt-post-drag)

    (bind-keys
     :map modi-mode-map
      ("C-S-o" . drag-stuff-up)
      ("C-S-l" . drag-stuff-down)
      ("C-:"   . drag-stuff-left)
      ("C-\""  . drag-stuff-right))

    (drag-stuff-global-mode 1)))


(provide 'setup-drag-stuff)
