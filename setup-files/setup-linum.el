;; Time-stamp: <2020-06-26 16:14:19 kmodi>

;; Line number package manager

;; Contents:
;;
;;  Native line number support (emacs 26+)
;;  linum
;;  nlinum

(defvar modi/linum-fn-default (>=e "26.0"
                                  'native-linum
                                'nlinum)
  "Default “linum” mode. This is used when toggling linum on and off.
Set this value to either `native-linum', `nlinum' or `linum'.")
;; (setq modi/linum-fn-default 'nlinum)

(defvar modi/linum--prev-state nil
  "State variable that tells if line numbers are being displayed.

If nil, the line numbers are not displayed. Otherwise this value
is either `native-linum', `nlinum' or `linum'.

This variable is meant to show only the current “linum” state; it
must not be set by the user.")

(defvar modi/linum-mode-enable-global nil
  "Variable to enable a “linum” mode globally or selectively
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
  "List of hooks of major modes in which a “linum” mode should be
  enabled.")

;;; Native line number support (emacs 26+)
(defvar modi/native-linum-default t
  "Value set for `display-line-numbers' when enabled.
Valid values are t, `visual', `relative' and nil. See
`display-line-numbers' for more information.")

(defun modi/native-linum--on (&optional global)
  "Enable native line number display in the current buffer.
If GLOBAL is non-nil, enable this globally."
  (interactive "P")
  (if global
      (setq-default display-line-numbers modi/native-linum-default)
    (setq-local display-line-numbers modi/native-linum-default)))

(defun modi/native-linum--off (&optional global)
  "Disable native line number display in the current buffer.
If GLOBAL is non-nil, disable this globally."
  (interactive "P")
  (if global
      (setq-default display-line-numbers nil)
    (setq-local display-line-numbers nil)))

(defun modi/turn-on-native-linum ()
  "Turn on native line numbers in specific modes.
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
        (add-hook hook #'modi/native-linum--on)))))

(defun modi/turn-off-native-linum ()
  "Turn off native line numbers in specific modes."
  (interactive)
  (modi/native-linum--off :global)
  (dolist (hook modi/linum-mode-hooks)
    (remove-hook hook #'modi/native-linum--on)))

;;; linum
(use-package linum
  :config
  (progn
    (defun modi/blend-linum ()
      "Set the linum foreground face to that of
`font-lock-comment-face' and background color to that of the
theme."
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

;;; nlinum
;; http://elpa.gnu.org/packages/nlinum.html
(use-package nlinum
  ;; :load-path "GNU-Elpa/packages/nlinum"
  :load-path "elisp/nlinum" ;Sticking to nlinum 1.7 for now because https://debbugs.gnu.org/cgi/bugreport.cgi?bug=29031#11
  :config
  (progn
    (setq nlinum-format " %d ")     ;1 space padding on each side of line number
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
          (when global-nlinum-mode
            (global-nlinum-mode -1))
          (dolist (hook modi/linum-mode-hooks)
            (add-hook hook #'nlinum-mode)))))

    (defun modi/turn-off-nlinum ()
      "Unhook nlinum mode from various major modes."
      (interactive)
      (global-nlinum-mode -1)
      (dolist (hook modi/linum-mode-hooks)
        (remove-hook hook #'nlinum-mode)))))

(defun modi/linum-set (linum-pkg)
  "Enable or disable linum.

With LINUM-PKG set to either `native-linum', `nlinum' or `linum',
the respective linum mode will be enabled.

When LINUM-PKG is `off' or nil, linum will be disabled altogether."
  (interactive
   (list (intern (completing-read
                  "linum pkg (default nlinum): "
                  '("native-linum" "nlinum" "linum" "off")
                  nil :require-match nil nil "nlinum"))))
  (when (stringp linum-pkg)
    (setq linum-pkg (intern linum-pkg)))
  (cl-case linum-pkg
    (native-linum
     (>=e "26.0"
         (progn
           (modi/turn-off-linum)
           (modi/turn-off-nlinum)
           (modi/turn-on-native-linum))
       (user-error "You needs emacs version 26.0 or newer to get the native line numbers feature")))
    (nlinum
     (>=e "26.0"
         (modi/turn-off-native-linum))
     (modi/turn-off-linum)
     (modi/turn-on-nlinum))
    (linum
     (>=e "26.0"
         (modi/turn-off-native-linum))
     (modi/turn-off-nlinum)
     (modi/turn-on-linum))
    (t                                  ;'off or nil
     (>=e "26.0"
         (modi/turn-off-native-linum))
     (modi/turn-off-linum)
     (modi/turn-off-nlinum)))
  (let ((activated-str (if (or (null linum-pkg)
                               (equal linum-pkg 'off))
                           ""
                         (format "Activated `%s'. " linum-pkg)))
        (deactivated-str (if modi/linum--prev-state
                             (format "Deactivated `%s'. " modi/linum--prev-state)
                           ""))
        (note-str "Revert buffer to see the change."))
    (message (concat activated-str deactivated-str note-str)))
  ;; Set the 'previous state' of linum
  (if (or (null linum-pkg)
          (equal linum-pkg 'off))
      (setq modi/linum--prev-state nil)
    (setq modi/linum--prev-state linum-pkg)))

(defun modi/linum--enable (&optional frame)
  "Set “linum” using the default package set by the user in
`modi/linum-fn-default'.

The optional FRAME argument is added as it is needed if this
function is added to the `after-make-frame-functions' hook."
  (modi/linum-set modi/linum-fn-default))

(defun modi/linum-toggle ()
  "Toggle “linum” between the disabled and enabled states."
  (interactive)
  (if modi/linum--prev-state
      (modi/linum-set 'off)
    (modi/linum--enable)))

;; Set linum
(if (daemonp)
    ;; Need to delay linum activation till the frame and fonts are loaded, only
    ;; for emacsclient launches. For non-daemon, regular emacs launches, the
    ;; frame is loaded *before* the emacs config is read. Not doing so results
    ;; in the below error in emacs 24.5:
    ;;   *ERROR*: Invalid face: linum
    (add-hook 'after-make-frame-functions #'modi/linum--enable)
  ;; Even when running in non-daemon mode, run `modi/linum--enable' only after
  ;; the init has loaded, so that the last modified value of
  ;; `modi/linum-fn-default' if any in setup-personal.el is the one effective,
  ;; not its standard value in its defvar form above.
  ;;
  ;; Mon Oct 30 12:50:52 EDT 2017 - kmodi
  ;; Use `window-setup-hook' instead of `after-init-hook', else emacs startup
  ;; freezes on emacs 25.x - https://debbugs.gnu.org/cgi/bugreport.cgi?bug=29031.
  ;; (add-hook 'after-init-hook #'modi/linum--enable) ;This does not work
  (add-hook 'window-setup-hook #'modi/linum--enable))


(provide 'setup-linum)
