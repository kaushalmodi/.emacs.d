;; Time-stamp: <2016-05-07 00:49:46 kmodi>

;; Line number package manager

(defvar modi/linum-fn-default 'nlinum
  "Default value of linum mode, used when toggling linum on and off.
Set this value to either 'nlinum, 'linum or 'linum-relative.")

(defvar modi/linum-fn-internal nil
  "State variable that tells if line numbers are being displayed or not.
If nil, the line numbers are not displayed.
Else this value is either 'nlinum, 'linum or 'linum-relative.

This variable is for internal use only, not to be set by user.")

(defvar modi/linum-mode-enable-global nil
  "Variable to enable a linum mode globally or selectively based on major modes.")

(defconst modi/linum-mode-hooks '(verilog-mode-hook
                                  emacs-lisp-mode-hook
                                  cperl-mode-hook
                                  c-mode-hook
                                  python-mode-hook
                                  matlab-mode-hook
                                  sh-mode-hook
                                  web-mode-hook
                                  html-mode-hook
                                  css-mode-hook
                                  makefile-gmake-mode-hook
                                  tcl-mode-hook
                                  conf-space-mode-hook
                                  d-mode-hook)
  "List of hooks of major modes in which a linum mode should be enabled.")

;; linum
(use-package linum
  :config
  (progn
    (defun modi/blend-linum ()
      "Set the linum foreground face to that of `font-lock-comment-face' and
background color to that of the theme."
      (interactive)
      (set-face-attribute
       'linum nil
       :height 0.9
       :foreground (if (string= (face-foreground 'font-lock-comment-face)
                                "unspecified-fg")
                       "#8f8f8f"
                     (face-foreground 'font-lock-comment-face))
       :background (if (string= (face-background 'default)
                                "unspecified-bg")
                       "#282828"
                     (face-background 'default))))

    (defun modi/turn-on-linum ()
      "Turn on linum mode in specific modes."
      (interactive)
      (if modi/linum-mode-enable-global
          (progn
            (dolist (hook modi/linum-mode-hooks)
              (remove-hook hook #'linum-mode))
            (global-linum-mode 1))
        (progn
          (when global-linum-mode
            (global-linum-mode -1))
          (dolist (hook modi/linum-mode-hooks)
            (add-hook hook #'linum-mode)))))

    (defun modi/turn-off-linum ()
      "Unhook linum mode from various major modes."
      (interactive)
      (global-linum-mode -1)
      (dolist (hook modi/linum-mode-hooks)
        (remove-hook hook #'linum-mode)))))

;; linum relative
(use-package linum-relative
  :config
  (progn
    ;; The symbol you want to show on the current line, by default it is 0.
    ;; You can use any string like \"->\". If this variable is empty string,
    ;; linum-releative will show the real line number at current line.
    (setq linum-relative-current-symbol "")))

;; nlinum
;; http://elpa.gnu.org/packages/nlinum.html
(use-package nlinum
  :config
  (progn
    (setq nlinum-format " %d ") ; 1 space padding on each side of line number

    (defun modi/turn-on-nlinum ()
      "Turn on nlinum mode in specific modes."
      (interactive)
      (if modi/linum-mode-enable-global
          (progn
            (dolist (hook modi/linum-mode-hooks)
              (remove-hook hook #'nlinum-mode))
            (global-nlinum-mode 1))
        (progn
          (when global-linum-mode
            (global-nlinum-mode -1))
          (dolist (hook modi/linum-mode-hooks)
            (add-hook hook #'nlinum-mode)))))

    (defun modi/turn-off-nlinum ()
      "Unhook nlinum mode from various major modes."
      (interactive)
      (global-nlinum-mode -1)
      (dolist (hook modi/linum-mode-hooks)
        (remove-hook hook #'nlinum-mode)))))

(defun modi/set-linum (linum-pkg)
  "Enable or disable linum.
With LINUM-PKG set to either 'nlinum, 'linum-relative or 'linum, the
respective linum mode will be enabled. When LINUM-PKG is nil, linum will be
disabled altogether."
  (interactive
   (list (intern (completing-read
                  "linum pkg (default nlinum): "
                  '("nlinum" "linum-relative" "linum" "nil")
                  nil t nil nil "nlinum"))))
  (when (stringp linum-pkg)
    (setq linum-pkg (intern linum-pkg)))
  (cl-case linum-pkg
    (nlinum
     (modi/turn-off-linum)
     (modi/turn-on-nlinum))
    (linum-relative
     (modi/turn-off-nlinum)
     (setq linum-format 'linum-relative)
     (modi/turn-on-linum))
    (linum
     (modi/turn-off-nlinum)
     (setq linum-format 'dynamic)
     (modi/turn-on-linum))
    (t
     (modi/turn-off-linum)
     (modi/turn-off-nlinum)))
  (if linum-pkg
      (message "Activated %s" linum-pkg)
    (when modi/linum-fn-internal
      (message "Deactivated %s" modi/linum-fn-internal)))
  (setq modi/linum-fn-internal linum-pkg))

(defun modi/toggle-linum (&optional frame)
  "Toggle linum between the disabled state and enabled using the default
package set by the user using variable `modi/linum-fn-default'.
The optional FRAME argument is added as it is needed when this frame is
added to the `after-make-frame-functions' hook."
  (interactive)
  (when frame
    (select-frame frame))
  (if modi/linum-fn-internal
      (modi/set-linum nil)
    (modi/set-linum modi/linum-fn-default)))

;; Set/unset linum
(add-hook 'after-make-frame-functions #'modi/toggle-linum)


(provide 'setup-linum)
