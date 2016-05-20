;; Time-stamp: <2016-05-20 18:06:00 kmodi>

;; Drag Stuff
;; https://github.com/rejeep/drag-stuff

(use-package drag-stuff
  :bind (:map modi-mode-map
         ("M-O" . drag-stuff-up)
         ("M-L" . drag-stuff-down) 
         ("M-K" . drag-stuff-left)
         ("M-\"" . drag-stuff-right))
  :config
  (progn
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
    (add-hook 'drag-stuff-after-drag-hook  #'modi/drag-stuff--rst-pt-post-drag)))


(provide 'setup-drag-stuff)
