;; -*- lexical-binding: t; -*-
;; Time-stamp: <2020-06-26 16:14:19 kmodi>

;; Line numbers using the built-in `display-line-numbers'

(defvar modi/linum--enabled nil
  "Non-nil when line numbers are being displayed.

This variable is meant to only show the current line number
state; it must not be set by the user.")

(defvar modi/linum-mode-enable-global nil
  "Variable to enable line numbers globally or selectively
based on major mode hooks added to the `modi/linum-mode-hooks'
variable.")

(defconst modi/linum-mode-hooks '(verilog-mode-hook
                                  emacs-lisp-mode-hook
                                  cperl-mode-hook
                                  c-mode-hook
                                  c++-mode-hook
                                  python-mode-hook
                                  matlab-mode-hook
                                  sh-mode-hook
                                  web-mode-hook
                                  html-mode-hook
                                  css-mode-hook
                                  makefile-gmake-mode-hook
                                  tcl-mode-hook
                                  conf-space-mode-hook
                                  conf-colon-mode-hook
                                  d-mode-hook
                                  sml-mode-hook
                                  nim-mode-hook
                                  nimscript-mode-hook
                                  go-mode-hook
                                  yaml-mode-hook)
  "List of hooks of major modes in which line numbers should be
  enabled.")

(defvar modi/native-linum-default t
  "Value set for `display-line-numbers' when enabled.
Valid values are t, `visual', `relative' and nil. See
`display-line-numbers' for more information.")

(defun modi/native-linum--on (&optional global)
  "Enable line number display in the current buffer.
If GLOBAL is non-nil, enable this globally."
  (interactive "P")
  (if global
      (setq-default display-line-numbers modi/native-linum-default)
    (setq-local display-line-numbers modi/native-linum-default)))

(defun modi/native-linum--off (&optional global)
  "Disable line number display in the current buffer.
If GLOBAL is non-nil, disable this globally."
  (interactive "P")
  (if global
      (setq-default display-line-numbers nil)
    (setq-local display-line-numbers nil)))

(defun modi/turn-on-native-linum ()
  "Turn on line numbers in specific modes.
In enabled state, `display-line-numbers' is set to
`modi/native-linum-default'."
  (interactive)
  (if modi/linum-mode-enable-global
      (progn
        (dolist (hook modi/linum-mode-hooks)
          (remove-hook hook #'modi/native-linum--on))
        (modi/native-linum--on :global))
    (progn
      (modi/native-linum--off :global)
      (dolist (hook modi/linum-mode-hooks)
        (add-hook hook #'modi/native-linum--on))))
  (setq modi/linum--enabled t))

(defun modi/turn-off-native-linum ()
  "Turn off line numbers in specific modes."
  (interactive)
  (modi/native-linum--off :global)
  (dolist (hook modi/linum-mode-hooks)
    (remove-hook hook #'modi/native-linum--on))
  (setq modi/linum--enabled nil))

(defun modi/linum--enable (&optional _frame)
  "Enable line numbers.

The optional FRAME argument is added as it is needed if this
function is added to the `after-make-frame-functions' hook."
  (modi/turn-on-native-linum))

(defun modi/linum-toggle ()
  "Toggle line numbers between the disabled and enabled states."
  (interactive)
  (if modi/linum--enabled
      (modi/turn-off-native-linum)
    (modi/linum--enable))
  (message "Line numbers %s. Revert buffer to see the change."
           (if modi/linum--enabled "enabled" "disabled")))

;; Run `modi/linum--enable' only after the init has loaded, so that the
;; last modified value of `modi/linum-mode-enable-global' if any in
;; setup-personal.el is the one effective, not its standard value in its
;; defvar form above.
(add-hook 'window-setup-hook #'modi/linum--enable)


(provide 'setup-linum)
