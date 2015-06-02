;; Time-stamp: <2015-06-02 17:26:11 kmodi>

;; Drag Stuff
;; https://github.com/rejeep/drag-stuff

(use-package drag-stuff
  :init
  (progn
    (setq drag-stuff-modifier nil))
  :config
  (progn
    ;; Solution posted by drag-stuff dev on posting an issue I faced,
    ;; https://github.com/rejeep/drag-stuff.el/issues/4
    (defvar drag-stuff-hax nil)
    (add-hook 'drag-stuff-before-drag-hook
              (lambda () (when (and (region-active-p) (zerop (current-column)))
                           (backward-char 1)
                           (setq drag-stuff-hax t))))
    (add-hook 'drag-stuff-after-drag-hook
              (lambda () (when drag-stuff-hax
                           (forward-char 1)
                           (setq drag-stuff-hax nil))))

    (bind-keys
     :map modi-mode-map
      ("C-M-'" . drag-stuff-up)
      ("C-M-/" . drag-stuff-down)
      ("C-:"   . drag-stuff-left)
      ("C-\""  . drag-stuff-right))

    (drag-stuff-global-mode 1)))


(provide 'setup-drag-stuff)
