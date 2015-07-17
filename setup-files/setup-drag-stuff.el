;; Time-stamp: <2015-07-17 09:23:05 kmodi>

;; Drag Stuff
;; https://github.com/rejeep/drag-stuff

(use-package drag-stuff
  :config
  (progn
    ;; Do not use `drag-stuff' keys
    (defun drag-stuff-define-keys ())
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
      ("C-S-o" . drag-stuff-up)
      ("C-S-l" . drag-stuff-down)
      ("C-:"   . drag-stuff-left)
      ("C-\""  . drag-stuff-right))

    (drag-stuff-global-mode 1)))


(provide 'setup-drag-stuff)
