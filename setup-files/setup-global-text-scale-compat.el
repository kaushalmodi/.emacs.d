;; Global text scale resizing for emacs 28 and older.

(with-eval-after-load 'setup-font-check
  (defvar default-font-size-pt
    (cond
     ((modi/is-font "Monoid") 11)
     ((modi/is-font "Pragmata") 13)
     ((modi/is-font "Iosevka SS08") 14)
     (t 12))
    "Default font size in points."))

(defun modi/global-font-size-adj (scale &optional absolute)
  "Adjust the font sizes globally: in all the buffers, mode line, echo area, etc.

The inbuilt `text-scale-adjust' function (bound to C-x C-0/-/= by default)
does an excellent job of font resizing. But it does not change the font sizes
of text outside the current buffer; for example, in the mode line.

M-<SCALE> COMMAND increases font size by SCALE points if SCALE is +ve,
                  decreases font size by SCALE points if SCALE is -ve
                  resets    font size if SCALE is 0.

If ABSOLUTE is non-nil, text scale is applied relative to the default font size
`default-font-size-pt'. Else, the text scale is applied relative to the current
font size."
  (interactive "p")
  (if (= scale 0)
      (setq font-size-pt default-font-size-pt)
    (if (bound-and-true-p absolute)
        (setq font-size-pt (+ default-font-size-pt scale))
      (setq font-size-pt (+ font-size-pt scale))))
  ;; The internal font size value is 10x the font size in points unit.
  ;; So a 10pt font size is equal to 100 in internal font size value.
  (set-face-attribute 'default nil :height (* font-size-pt 10)))

(defun modi/global-font-size-incr ()  (interactive) (modi/global-font-size-adj +1))
(defun modi/global-font-size-decr ()  (interactive) (modi/global-font-size-adj -1))
(defun modi/global-font-size-reset () (interactive) (modi/global-font-size-adj 0))

;; Initialize font-size-pt var to the default value
(with-eval-after-load 'setup-font-check
  (modi/global-font-size-reset))

;; Usage: C-c C-- = - 0 = = = = - - 0
;; Usage: C-c C-= = 0 - = - = = = = - - 0
(defhydra hydra-font-resize (nil
                             "C-c"
                             :bind (lambda (key cmd) (bind-key key cmd modi-mode-map))
                             :color red
                             :hint nil)
  "
Font Size:     _C--_/_-_ Decrease     _C-=_/_=_ Increase     _C-0_/_0_ Reset     _q_ Cancel
"
  ;; Hydra entry bindings
  ("C--" modi/global-font-size-decr)
  ("C-=" modi/global-font-size-incr)
  ("C-0" modi/global-font-size-reset :color blue)
  ;; Hydra-internal bindings.. below work only when the hydra is active!
  ("-"   modi/global-font-size-decr :bind nil)
  ("="   modi/global-font-size-incr :bind nil)
  ("+"   modi/global-font-size-incr :bind nil)
  ("0"   modi/global-font-size-reset :bind nil)
  ("q"   nil :color blue))

(bind-keys
 :map modi-mode-map
 ;; <C-down-mouse-1> is bound to `mouse-buffer-menu' by default. It is
 ;; inconvenient when that mouse menu pops up when I don't need it
 ;; to. And actually I have never used that menu :P
 ("<C-down-mouse-1>" . modi/global-font-size-reset) ;Ctrl + left mouse down event
 ("<C-mouse-1>" . modi/global-font-size-reset)      ;Ctrl + left mouse up event
 ;; Make Control+mousewheel do increase/decrease font-size
 ;; http://ergoemacs.org/emacs/emacs_mouse_wheel_config.html
 ("<C-mouse-4>" . modi/global-font-size-incr)  ;Ctrl + wheel-up
 ("<XF86AudioRaiseVolume>" . modi/global-font-size-incr) ;DasQ Q-wheel clockwise
 ("<C-mouse-5>" . modi/global-font-size-decr)            ;Ctrl + wheel-down
 ("<XF86AudioLowerVolume>" . modi/global-font-size-decr)  ;DasQ Q-wheel counter-clockwise
 )


(provide 'setup-global-text-scale-compat)
