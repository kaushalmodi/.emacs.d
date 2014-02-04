;; Time-stamp: <2014-01-21 14:39:42 kmodi>

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
