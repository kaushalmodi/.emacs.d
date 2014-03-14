;; Time-stamp: <2014-03-12 23:36:20 kmodi>

;; Drag Stuff
;; Source: https://github.com/rejeep/drag-stuff

(require 'drag-stuff)

;; Solution posted by drag-stuff dev on posting an issue I faced,
;; https://github.com/rejeep/drag-stuff.el/issues/4

(defvar drag-stuff-hax nil)

(add-hook 'drag-stuff-before-drag-hook
          (lambda ()
            (when (and (region-active-p) (zerop (current-column)))
              (backward-char 1)
              (setq drag-stuff-hax t))))

(add-hook 'drag-stuff-after-drag-hook
          (lambda ()
            (when drag-stuff-hax
              (forward-char 1)
              (setq drag-stuff-hax nil))))
;; end of solution

;; Drag Stuff is incompatible with Org, because it shadows many useful Org
;; bindings.  This doesn't do much harm, because Org has its own structural
;; movement commands
(add-to-list 'drag-stuff-except-modes 'org-mode)

(setq drag-stuff-global-mode t)


(setq setup-drag-stuff-loaded t)
(provide 'setup-drag-stuff)

;; Drag line
;; To drag a line up and down. Put the cursor on that line and press <C-S-up> and
;; <C-S-down>.

;; Drag lines
;; To drag several lines up and down. Select the lines you want to drag and
;; press <C-S-up> and <C-S-down>.

;; Drag region
;; A region can be dragged to the left and right. Select the region you want to
;; drag and press <C-S-left> and <C-S-right>.

;; Drag word
;; To drag a word. Place the cursor on the word and press <C-S-left> and <C-S-right>.
