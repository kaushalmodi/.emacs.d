;; Time-stamp: <2016-07-28 00:28:08 kmodi>

;; Line number package manager

(defvar modi/linum-fn-default 'nlinum
  "Default “linum” mode. This is used when toggling linum on and off.
Set this value to either `nlinum' or `linum'.")

(defvar modi/linum--state nil
  "State variable that tells if line numbers are being displayed or not.

If nil, the line numbers are not displayed. Otherwise this value is either
`nlinum' or `linum'.

This variable is meant to show only the current “linum” state; it must not
be set by the user.")

(defvar modi/linum-mode-enable-global nil
  "Variable to enable a “linum” mode globally or selectively based on major
mode hooks added to the `modi/linum-mode-hooks' variable.")

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
                                  d-mode-hook
                                  sml-mode-hook)
  "List of hooks of major modes in which a “linum” mode should be enabled.")

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
       :foreground (if (string= (face-foreground 'font-lock-comment-face) "unspecified-fg")
                       "#8f8f8f"
                     (face-foreground 'font-lock-comment-face))
       :background (if (string= (face-background 'default) "unspecified-bg")
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

;; nlinum
;; http://elpa.gnu.org/packages/nlinum.html
(use-package nlinum
  :load-path "elisp/nlinum"
  :config
  (progn
    (setq nlinum-format " %d ") ; 1 space padding on each side of line number
    (setq nlinum-highlight-current-line t)

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
With LINUM-PKG set to either 'nlinum or 'linum, the
respective linum mode will be enabled. When LINUM-PKG is nil, linum will be
disabled altogether."
  (interactive
   (list (intern (completing-read
                  "linum pkg (default nlinum): "
                  '("nlinum" "linum" "nil")
                  nil t nil nil "nlinum"))))
  (when (stringp linum-pkg)
    (setq linum-pkg (intern linum-pkg)))
  (cl-case linum-pkg
    (nlinum
     (modi/turn-off-linum)
     (modi/turn-on-nlinum))
    (linum
     (modi/turn-off-nlinum)
     (modi/turn-on-linum))
    (t
     (modi/turn-off-linum)
     (modi/turn-off-nlinum)))
  (let (state-str filler-str)
    (when linum-pkg
      (setq state-str (format "Activated `%s'." linum-pkg)))
    (when modi/linum--state
      (when state-str
        (setq filler-str " "))
      (setq state-str (concat (format "Deactivated `%s'." modi/linum--state)
                              filler-str state-str)))
    (message (format "%s Revert buffer to see the change." state-str)))
  (setq modi/linum--state linum-pkg))

(defun modi/toggle-linum (&optional frame)
  "Toggle “linum” between the disabled and enabled states using the default
package set by the user in `modi/linum-fn-default'.

The optional FRAME argument is added as it is needed when this function is
added to the `after-make-frame-functions' hook."
  (interactive)
  (if modi/linum--state
      (modi/set-linum nil)
    (modi/set-linum modi/linum-fn-default)))

;; Set/unset linum
(if (daemonp)
    ;; Need to delay linum activation till the frame and fonts are loaded, only
    ;; for emacsclient launches. For non-daemon, regular emacs launches, the
    ;; frame is loaded *before* the emacs config is read. Not doing so results
    ;; in the below error in emacs 24.5:
    ;;   *ERROR*: Invalid face: linum
    (add-hook 'after-make-frame-functions #'modi/toggle-linum)
  ;; Even when running in non-daemon mode, run `modi/toggle-linum' only after the
  ;; init has loaded, so that the last modified value of `modi/linum-fn-default'
  ;; if any in setup-personal.el is the one effective, not its standard value
  ;; in its defvar form above.
  (add-hook 'after-init-hook #'modi/toggle-linum))


(provide 'setup-linum)
