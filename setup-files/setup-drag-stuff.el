;; Time-stamp: <2015-02-23 11:42:32 kmodi>

;; Drag Stuff
;; Source: https://github.com/rejeep/drag-stuff

(use-package drag-stuff
  :config
  (progn
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Solution posted by drag-stuff dev on posting an issue I faced,
    ;; https://github.com/rejeep/drag-stuff.el/issues/4
    (defvar drag-stuff-hax nil)
    (add-hook 'drag-stuff-before-drag-hook
              (λ (when (and (region-active-p) (zerop (current-column)))
                   (backward-char 1)
                   (setq drag-stuff-hax t))))
    (add-hook 'drag-stuff-after-drag-hook
              (λ (when drag-stuff-hax
                   (forward-char 1)
                   (setq drag-stuff-hax nil))))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Drag Stuff is incompatible with Org, because it shadows many useful Org
    ;; bindings.  This doesn't do much harm, because Org has its own structural
    ;; movement commands
    (add-to-list 'drag-stuff-except-modes 'org-mode)
    (setq drag-stuff-global-mode t)
    (bind-keys
     :map modi-mode-map
     ("C-\""        . drag-stuff-up)   ;; C-S-'
     ("C-?"         . drag-stuff-down) ;; C-S-/
     ("<C-S-left>"  . drag-stuff-left)
     ("<C-S-right>" . drag-stuff-right))))


(provide 'setup-drag-stuff)
