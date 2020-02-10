;; Time-stamp: <2020-01-20 13:52:35 kmodi>

;; Set up the looks of emacs

;; Contents:
;;
;;  Variables
;;  Show Paren
;;  Bars, Dividers, and Window Elements
;;    Menu bar
;;    Tool bar
;;    Scroll bar
;;    Window Dividers
;;  Themes
;;  Frame Title
;;  Fonts
;;    Font Lock
;;      Syntax highlight .vimrc files (I know, blasphemy!)
;;    Fix italics
;;    Windows Font
;;    Global Font Resize
;;  Line truncation
;;  Visual Line Mode
;;    Adaptive Wrap
;;  Cursor
;;  Prez Mode
;;  Hidden Mode Line Mode
;;  Show mode line in header
;;  Fringes
;;  Coloring regions with ANSI color codes
;;  Whitespace Mode/Show Long Lines
;;  Narrow/Widen
;;  Prettify symbols
;;  Visually differentiate confusing characters

;;; Variables
(setq inhibit-startup-message t)     ;No splash screen at startup
(setq scroll-step 1)                 ;Scroll 1 line at a time
(setq tooltip-mode nil)              ;Disable tooltip appearance on mouse hover
(setq frame-resize-pixelwise t)      ;Allow frame size to inc/dec by a pixel
(setq visible-bell t)    ;Enable visible bell or screen blink to happen on error

(defun modi/is-font (fontname)
  "Return non-nil if the default font matches FONTNAME."
  ;; http://superuser.com/a/1100378/209371
  (string-match-p fontname (format "%s" (face-attribute 'default :font))))

(with-eval-after-load 'setup-font-check
  (defvar default-font-size-pt
    (cond
     ((modi/is-font "Monoid") 11)
     ((modi/is-font "Pragmata") 13)
     ((modi/is-font "Iosevka SS08") 14)
     (t 12))
    "Default font size in points."))

(defvar dark-theme t
  "Variable to store the nature of theme whether it is light or dark.
This variable is to be updated when changing themes.")

;;; Show Paren
;; Highlight closing parentheses; show the name of the body being closed with
;; the closing parentheses in the minibuffer.
(show-paren-mode 1)

;;; Bars, Dividers, and Window Elements

;;;; Menu bar
;; Do not show the menu bar with File|Edit|Options|...
(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))

;; Toggle menu bar
(>=e "25.0"
    (progn
      ;; Do not resize the frame when `menu-bar-mode' is toggled.
      (add-to-list 'frame-inhibit-implied-resize 'menu-bar-lines) ;Default nil on GTK+
      (bind-key "<f2>" #'menu-bar-mode modi-mode-map)
      (key-chord-define-global "2w" #'menu-bar-mode)) ;Alternative to F2
  (progn
    (defvar bkp--frame-text-height-px (frame-text-height)
      "Backup of the frame text height in pixels.")
    (defvar bkp--frame-text-width-px (frame-text-width)
      "Backup of the frame text width in pixels.")

    (defun modi/toggle-menu-bar ()
      "Toggle the menu bar.
Also restore the original frame size when disabling the menu bar."
      (interactive)
      (let ((frame-resize-pixelwise t))
        ;; If the menu bar is hidden currently, take a backup of the frame height.
        (when (null menu-bar-mode)
          ;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=21480
          (setq bkp--frame-text-height-px (frame-text-height))
          (setq bkp--frame-text-width-px (frame-text-width)))
        (menu-bar-mode 'toggle)
        ;; Restore frame size if menu bar is hidden after toggle
        (when (null menu-bar-mode)
          (set-frame-size nil bkp--frame-text-width-px bkp--frame-text-height-px :pixelwise))))
    (bind-key "<f2>" #'modi/toggle-menu-bar modi-mode-map)
    (key-chord-define-global "2w" #'modi/toggle-menu-bar))) ;Alternative to F2

;;;; Tool bar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))            ;Do not show the tool bar with icons on the top
(>=e "25.0"
    ;; Do not resize the frame when toggling `tool-bar-mode'
    (add-to-list 'frame-inhibit-implied-resize 'tool-bar-lines))

;;;; Scroll bar
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))                 ;Disable the scroll bars

;;;; Window Dividers
(use-package frame
  :defer t
  :config
  (progn
    (setq window-divider-default-places 'right-only) ;Default 'right-only
    ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=27830#20
    ;; Workaround on emacs 26+ to prevent fringe truncation. You need to use
    ;; either scroll bars or window dividers to prevent that.
    ;; I dislike the default face of `window-divider', so I customize that in my
    ;; `smyx-theme`.
    (setq window-divider-default-right-width 1) ;Default 6
    (window-divider-mode 1)))

;;; Themes
;;                     THEME-NAME      DARK   FCI-RULE-COLOR
(defconst my/themes '((smyx            'dark  "gray40")
                      (zenburn         'dark  "gray40")
                      (darktooth       'dark  "gray40")
                      (ample           'dark  "gray40")
                      (ample-flat      'dark  "gray40")
                      (planet          'dark  "gray40")
                      (tao-yin         'dark  "gray40")
                      (tao-yang        'light "gray")
                      (ample-light     'light "gray")
                      (leuven          'light "gray")
                      (twilight-bright 'light "gray")
                      (default         'light "gray")) ;Default emacs theme
  "Alist of themes I tend to switch to frequently.")

(defun my/disable-enabled-themes ()
  "Disable all enable themes except the one used by `smart-mode-line'.

This function is not meant for interactive use. A clean way to disable all
themes will be to run `M-x load-theme/default' (this function is generated
by the `modi/gen-all-theme-fns' macro. That will ensure that all
themes are disabled and also fix the faces for linum, fringe, etc."
  (dolist (theme custom-enabled-themes)
    (unless (string-match "smart-mode-line-" (format "%s" theme))
      (disable-theme theme))))

;; How can I create multiple defuns by looping through a list?
;; http://emacs.stackexchange.com/a/10122/115
(defun modi/gen-theme-fn (theme-name dark fci-rule-color)
  "Function to generate a function to disable all themes and enable the chosen
theme, while also customizing few faces outside the theme.

The theme loading functions are named “load-theme/THEME-NAME”.
Example: For `smyx' theme, the generated function will be `load-theme/smyx'.

The DARK variable should be set to `'dark' if the theme is dark and `'light'
if otherwise.

The FCI-RULE-COLOR is the color string to set the color for fci rules.

Running `M-x load-theme/default' will disable all custom themes except
the smart-mode-line theme."
  (let ((theme-fn-name (intern (format "load-theme/%s" theme-name))))
    `(defun ,theme-fn-name ()
       (interactive)
       ;; `dark-theme' is set to `t' if `dark' value is `'dark'
       (setq dark-theme (equal ,dark 'dark))
       (my/disable-enabled-themes)
       (when (not (equal ',theme-name 'default))
         (load-theme ',theme-name :no-confirm))
       (with-eval-after-load 'general
         (modi/blend-fringe))
       (with-eval-after-load 'setup-linum
         (modi/blend-linum))
       (with-eval-after-load 'smart-mode-line
         (sml/apply-theme ,dark nil :silent)) ;Apply sml theme silently
       (when (not (bound-and-true-p disable-pkg-fci))
         (with-eval-after-load 'setup-fci
           ;; Below commented code does not work
           ;; (setq fci-rule-color (face-foreground 'font-lock-comment-face))
           (setq fci-rule-color ,fci-rule-color)
           (when (fboundp 'modi/fci-redraw-frame-all-buffers)
             (modi/fci-redraw-frame-all-buffers)))))))

(defmacro modi/gen-all-theme-fns ()
  `(progn ,@(mapcar
             (lambda (x) (modi/gen-theme-fn (nth 0 x) (nth 1 x) (nth 2 x)))
             my/themes)))

(modi/gen-all-theme-fns)
;; (pp (macroexpand '(modi/gen-all-theme-fns))) ;For debug

(defconst default-dark-theme-fn  'load-theme/smyx
  "Function to set the default dark theme.")
(defconst default-light-theme-fn 'load-theme/leuven
  "Function to set the default light theme.")
(defconst default-theme-fn default-dark-theme-fn
  "Function to set the default theme.")

(defun toggle-theme ()
  "Toggles theme between the default light and default dark themes."
  (interactive)
  (if dark-theme
      (funcall default-light-theme-fn)
    (funcall default-dark-theme-fn)))

;; Load the theme ONLY after the frame has finished loading (needed especially
;; when running emacs in daemon mode)
;; https://github.com/Malabarba/smart-mode-line/issues/84#issuecomment-46429893
;; ;; `after-make-frame-functions' hook is not run in no-window mode
;; (add-hook 'after-make-frame-functions (lambda (&rest frame)
;;                                         (funcall default-theme-fn)))
(add-hook 'window-setup-hook (lambda () (funcall default-theme-fn)))

;;; Frame Title
(defun modi/update-frame-title ()
  "Show the emacs branch/version and a useful buffer name in the frame title.

If the buffer is showing a file, show the full file name.
Else if it's showing a `dired' buffer, show the directory name.
Else show just the buffer name.

At the end, % is shown if the buffer is read-only,
            * is shown if the buffer has been modified, and
            - is shown if the buffer is editable, but not yet modified.

See `mode-line-format' to get help on the %-identifers used in this function."
  (setq frame-title-format
        `("emacs "
          ;; If `emacs-git-branch' is non-nil, show that
          (emacs-git-branch ,(concat "[" emacs-git-branch "]")
                            ;; Else show the version number
                            ,(concat (number-to-string emacs-major-version)
                                     "."
                                     (number-to-string emacs-minor-version)))
          "   "
          (buffer-file-name "%f" ;Show full file path if buffer is showing a file
                            (dired-directory dired-directory ;Else if in dired mode, show the directory name
                                             "%b")) ;Else show the buffer name (*scratch*, *Messages*, etc)
          "%*"))) ;Prints %(read-only), *(modified) or -(editable,not modified)
                                        ;to show the current buffer status.
(add-hook 'after-init-hook #'modi/update-frame-title)

;;; Fonts

;;;; Font Lock
;; Enable font-lock or syntax highlighting globally
(global-font-lock-mode 1)
;; Use the maximum decoration level available for color highlighting
(setq font-lock-maximum-decoration t)

;;;;; Syntax highlight .vimrc files (I know, blasphemy!)
;; http://stackoverflow.com/a/4238738/1219634
(define-generic-mode 'vimrc-generic-mode
  '()
  '()
  '(("^[\t ]*:?\\(!\\|ab\\|map\\|unmap\\)[^\r\n\"]*\"[^\r\n\"]*\\(\"[^\r\n\"]*\"[^\r\n\"]*\\)*$"
     (0 font-lock-warning-face))
    ("\\(^\\|[\t ]\\)\\(\".*\\)$"
     (2 font-lock-comment-face))
    ("\"\\([^\n\r\"\\]\\|\\.\\)*\""
     (0 font-lock-string-face)))
  '("/vimrc\\'" "\\.vim\\(rc\\)?\\'")
  '((lambda ()
      (modify-syntax-entry ?\" ".")))
  "Generic mode for Vim configuration files.")

(setq auto-mode-alist (append '(("\\.vimrc.*\\'" . vimrc-generic-mode)
                                ("\\.vim\\'"     . vimrc-generic-mode))
                              auto-mode-alist))

;;;; Fix italics
;; Make the italics show as actual italics. For some unknown reason, the below
;; is needed to render the italics in org-mode. The issue could be related to
;; the fonts in use. But having this doesn't hurt regardless.
(set-face-attribute 'italic nil :inherit nil :slant 'italic)

;;;; Windows Font
(when (eq system-type 'windows-nt)
  (set-face-attribute 'default nil :family "Consolas"))

;;;; Global Font Resize
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
 ("<XF86AudioLowerVolume>" . modi/global-font-size-decr)) ;DasQ Q-wheel counter-clockwise

(>=e "25.0"
    ;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=21480
    ;; Do not resize the frame when adjusting the font size
    (add-to-list 'frame-inhibit-implied-resize 'font))

;;; Line truncation
;; Enable truncation. This setting does NOT apply to windows split using `C-x 3`
(setq-default truncate-lines t)
;; Do `M-x toggle-truncate-lines` to toggle truncation mode.
;; `truncate-partial-width-windows' has to be nil for `toggle-truncate-lines'
;; to work even in split windows
(setq-default truncate-partial-width-windows nil)

(bind-key "C-x t" #'toggle-truncate-lines modi-mode-map)

;;; Visual Line Mode
;; Do word wrapping only at word boundaries
(defconst modi/visual-line-mode-hooks '(org-mode-hook
                                        markdown-mode-hook)
  "List of hooks of major modes in which visual line mode should be enabled.")

(defun modi/turn-on-visual-line-mode ()
  "Turn on visual-line-mode only for specific modes."
  (interactive)
  (dolist (hook modi/visual-line-mode-hooks)
    (add-hook hook #'visual-line-mode)))

(defun modi/turn-off-visual-line-mode ()
  "Turn off visual-line-mode only for specific modes."
  (interactive)
  (dolist (hook modi/visual-line-mode-hooks)
    (remove-hook hook #'visual-line-mode)))

(modi/turn-on-visual-line-mode)

;; Turn on line wrapping fringe indicators in Visual Line Mode
(setq-default visual-line-fringe-indicators '(left-curly-arrow
                                              right-curly-arrow))

;;;; Adaptive Wrap
;; `adaptive-wrap-prefix-mode' indents the visual lines to
;; the level of the actual line plus `adaptive-wrap-extra-indent'. Thus line
;; truncation has to be off for adaptive wrap to be in effect.
(use-package adaptive-wrap
  :defer t
  :config
  (progn
    ;; Need to set the below variable globally as it is a buffer-local variable.
    (setq-default adaptive-wrap-extra-indent 2)

    ;; Adaptive wrap anyways needs the `visual-line-mode' to be enabled. So
    ;; enable it only when the latter is enabled.
    (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)))

;;; Cursor
;; Change cursor color according to mode:
;;   read-only buffer / overwrite / regular (insert) mode
(blink-cursor-mode -1)                ;Don't blink the cursor, it's distracting!

(defvar hcz-set-cursor-color-color "")
(defvar hcz-set-cursor-color-buffer "")
(defun hcz-set-cursor-color-according-to-mode ()
  "change cursor color according to some minor modes."
  ;; set-cursor-color is somewhat costly, so we only call it when needed:
  (let ((color
         (if buffer-read-only
             ;; Color when the buffer is read-only
             (progn (if dark-theme "yellow" "dark orange"))
           ;; Color when the buffer is writeable but overwrite mode is on
           (if overwrite-mode "red"
             ;; Color when the buffer is writeable but overwrite mode if off
             (if dark-theme "white" "gray")))))
    (unless (and
             (string= color hcz-set-cursor-color-color)
             (string= (buffer-name) hcz-set-cursor-color-buffer))
      (set-cursor-color (setq hcz-set-cursor-color-color color))
      (setq hcz-set-cursor-color-buffer (buffer-name)))))
(add-hook 'post-command-hook #'hcz-set-cursor-color-according-to-mode)

;;; Prez Mode
(defvar prez-mode--buffer-name nil
  "Variable to store the name of the buffer in which the `prez-mode' was enabled.")

(defvar prez-mode--frame-configuration nil
  "Variable to store the frame configuration before `prez-mode' was enabled.")

(define-minor-mode prez-mode
  "Minor mode for presentations.

- The frame size is reduced.
- All windows other than the current one are deleted.
- Font size is increased.
- Theme is toggled from the default dark theme to light theme.

Toggling off this mode reverts everything to their original states."
  :init-value nil
  :lighter    " Prez"
  (if prez-mode
      ;; Enable prez mode
      (progn
        (setq prez-mode--buffer-name (buffer-name))
        (setq prez-mode--frame-configuration (current-frame-configuration))
        (set-frame-size nil 110 40)     ;Rows and columns w h
        (delete-other-windows)
        (modi/global-font-size-adj +3 :absolute)
        (toggle-theme))
    ;; Disable prez mode
    (progn
      (set-frame-configuration prez-mode--frame-configuration)
      (switch-to-buffer prez-mode--buffer-name)
      (modi/global-font-size-reset)
      (toggle-theme))))
(define-globalized-minor-mode global-prez-mode prez-mode prez-mode)

;; F8 key can't be used as it launches the VNC menu
;; It can though be used with shift/ctrl/alt keys
(bind-key "<S-f8>" #'prez-mode modi-mode-map)
(key-chord-define-global "8i" #'prez-mode) ;Alternative to S-F8

;;; Hidden Mode Line Mode
;; (works only when one window is open)
;; FIXME: Make this activate only if one window is open
;; See http://bzg.fr/emacs-hide-mode-line.html
(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global nil
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x hidden-mode-line-mode RET to make the mode-line appear."))))

;; ;; Activate hidden-mode-line-mode
;; (hidden-mode-line-mode 1)

;;; Show mode line in header
;; http://bzg.fr/emacs-strip-tease.html
;; Careful: you need to deactivate hidden-mode-line-mode
(defun bzg/mode-line-in-header ()
  (interactive)
  (if (not header-line-format)
      (setq header-line-format mode-line-format)
    (setq header-line-format nil))
  (force-mode-line-update))

;;; Fringes
;; Show the top/bottom buffer boundaries only in the right fringe
(setq-default indicate-buffer-boundaries '((top . right)
                                           (bottom . right)))

;;; Coloring regions with ANSI color codes
;; http://unix.stackexchange.com/a/19505/57923
(defun ansi-color-apply-on-region-int (beg end)
  "Colorize using the ANSI color codes."
  (interactive "r")
  (ansi-color-apply-on-region beg end))

;;; Whitespace Mode/Show Long Lines
(use-package whitespace
  :defer t
  :config
  (progn
    (setq whitespace-line-column nil)  ;When nil, set the value to `fill-column'
    (setq whitespace-style
          '(face
            trailing                    ;White space at end of lines
            tabs                        ;tab-mark ;`tab-mark' shows tabs as '»'
            spaces space-mark           ;`space-mark' shows spaces as '.'
            space-before-tab space-after-tab ;Mix of tabs and spaces
            ;; lines   ;highlight lines that extend beyond `whitespace-line-column'
            lines-tail ;highlight only characters beyond `whitespace-line-column'
            ;; newline newline-mark
            ;; empty ;blank lines at BOB or EOB
            indentation)) ;highlight spaces/tabs at BOL depending on `indent-tabs-mode'

    ;; Do word wrapping only at word boundaries
    (defconst modi/whitespace-mode-hooks '(verilog-mode-hook
                                           emacs-lisp-mode-hook)
      "List of hooks of major modes in which whitespace-mode should be enabled.")

    (defun modi/turn-on-whitespace-mode ()
      "Turn on whitespace-mode only for specific modes."
      (interactive)
      (dolist (hook modi/whitespace-mode-hooks)
        (add-hook hook #'whitespace-mode)))

    (defun modi/turn-off-whitespace-mode ()
      "Turn off whitespace-mode only for specific modes."
      (interactive)
      (dolist (hook modi/whitespace-mode-hooks)
        (remove-hook hook #'whitespace-mode)))))

;;; Narrow/Widen
;; http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
(defun endless/narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or defun,
whichever applies first. Narrowing to org-src-block actually
calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer is already
narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p))
         (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode)
         (cond
          ((ignore-errors (org-edit-src-code) t))
          ((ignore-errors (org-narrow-to-block) t))
          (t
           (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t
         (narrow-to-defun))))
;; This line actually replaces Emacs' entire narrowing keymap.
;; This will also override the default "C-x n" binding in LaTeX-mode-map as
;; the below binding is done in `modi-mode-map'.
(bind-key "C-x n" #'endless/narrow-or-widen-dwim modi-mode-map)

;;; Prettify symbols
(defvar modi/prettify-symbols-mode-hooks '(prog-mode-hook
                                           text-mode-hook)
  "List of hooks of major modes in which prettify-symbols-mode should be enabled.")

(>=e "25.0"
    ;; Temporarily unprettify the symbol if the cursor is on the symbol or on
    ;; its right edge.
    (setq prettify-symbols-unprettify-at-point 'right-edge))

(dolist (hook modi/prettify-symbols-mode-hooks)
  (add-hook hook #'prettify-symbols-mode))

;; Show "(lambda ..)" as "(λ ..)" in lisp modes when `prettify-symbols-mode'
;; is enabled.
(setq lisp-prettify-symbols-alist '(("lambda" . ?λ)))

(with-eval-after-load 'setup-font-check
  (when (modi/is-font "Pragmata")
    (require 'setup-pragmata-ligatures)))

;;; Visually differentiate confusing characters
;; https://emacs.stackexchange.com/a/9627/115
(defface modi/highlight-confusing
  '((t (:foreground "black"
        :background "#b0b0b0")))
  "Face used to highlight confusing characters.
Used in `modi/highlight-confusing-chars'.")

(defun modi/highlight-confusing-chars ()
  "Highlight confusing characters in different glyphs + face.

EN DASH          -> 2 Hyphens
EM DASH          -> 3 Hyphens
ZERO WIDTH SPACE -> ∅

All glyphs are shown in `modi/highlight-confusing' face."
  (let* ((glyph-en-dash (make-glyph-code ?- 'modi/highlight-confusing)) ;HYPHEN-MINUS follows that ?
         (glyph-em-dash glyph-en-dash)
         (glyph-zws (make-glyph-code ?∅ 'modi/highlight-confusing))) ;EMPTY SET follows that ?
    (when (not buffer-display-table)
      (setq buffer-display-table (make-display-table)))
    (aset buffer-display-table ?– `[,glyph-en-dash ,glyph-en-dash]) ;EN DASH follows that ?
    (aset buffer-display-table ?— `[,glyph-em-dash ,glyph-em-dash ,glyph-em-dash]) ;EM DASH follows that ?
    (aset buffer-display-table ?​ `[,glyph-zws]))) ;ZERO WIDTH SPACE follows that ?
(dolist (hook '(prog-mode-hook
                org-mode-hook))
  (add-hook hook #'modi/highlight-confusing-chars))


(provide 'setup-visual)

;; Use the interactive function `fringe-mode' or `set-fringe-mode' (in elisp) to
;; tweak the fringe widths. Do not set the `fringe-mode' variable directly!
