;; Time-stamp: <2015-05-12 16:11:49 kmodi>

;; Set up the looks of emacs

;; Highlight closing parentheses; show the name of the body being closed with
;; the closing parentheses in the minibuffer.
(show-paren-mode 1)

(defvar default-font-size-pt 13
  "Default font size in points.")

(defvar dark-theme t
  "Variable to store the nature of theme whether it is light or dark.
This variable is to be updated when changing themes.")

(setq frame-resize-pixelwise t) ; allow frame size to inc/dec by a pixel

;; MENU/TOOL/SCROLL BARS
;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode)   (menu-bar-mode -1)) ; do not show the menu bar with File|Edit|Options|...
(if (fboundp 'tool-bar-mode)   (tool-bar-mode -1)) ; do not show the tool bar with icons on the top
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)) ; disable the scroll bars

(setq inhibit-startup-message t ; No splash screen at startup
      scroll-step 1 ; scroll 1 line at a time
      tooltip-mode nil ; disable tooltip appearance on mouse hover
      )

;;                     THEME-NAME      DARK   FCI-RULE-COLOR
(defconst my/themes '((smyx            'dark  "gray40")
                      (zenburn         'dark  "gray40")
                      (darktooth       'dark  "gray40")
                      (ample           'dark  "gray40")
                      (ample-flat      'dark  "gray40")
                      (planet          'dark  "gray40")
                      (ample-light     'light "gray")
                      (leuven          'light "gray")
                      (twilight-bright 'light "gray")
                      (default         'light "gray")) ; default emacs theme
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
         (load-theme ',theme-name t))
       (when (featurep 'defuns)
         (modi/blend-fringe))
       (when (featurep 'setup-linum)
         (modi/blend-linum))
       (when (featurep 'smart-mode-line)
         (sml/apply-theme ,dark nil :silent)) ; apply sml theme silently
       (when (featurep 'setup-fci)
         ;; Below commented code does not work
         ;; (setq fci-rule-color (face-foreground 'font-lock-comment-face))
         (setq fci-rule-color ,fci-rule-color)
         (modi/fci-redraw-frame-all-buffers)))))

(defmacro modi/gen-all-theme-fns ()
  `(progn ,@(mapcar
             (lambda (x) (modi/gen-theme-fn (nth 0 x) (nth 1 x) (nth 2 x)))
             my/themes)))

(modi/gen-all-theme-fns)
;; (pp (macroexpand '(modi/gen-all-theme-fns))) ; for debug

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
(add-hook 'window-setup-hook (λ (funcall default-theme-fn)))

(defun modi/update-frame-title ()
  (interactive)
  ;; Frame title bar format
  ;; If buffer-file-name exists, show it;
  ;; else if you are in dired mode, show the directory name
  ;; else show only the buffer name (*scratch*, *Messages*, etc)
  (setq frame-title-format (list '(buffer-file-name
                                   "%f"
                                   (dired-directory dired-directory "%b")))))
(add-hook 'after-init-hook #'modi/update-frame-title)

;; Enable font-lock or syntax highlighting globally
(global-font-lock-mode 1)
;; Use the maximum decoration level available for color highlighting
(setq font-lock-maximum-decoration t)

;; FONTS
;; Make the italics show as actual italics. For some unknown reason, the below
;; is needed to render the italics in org-mode. The issue could be related to
;; the fonts in use. But having this doesn't hurt regardless.
(set-face-attribute 'italic nil :inherit nil :slant 'italic)

;; Default fonts for Windows
(when (eq system-type 'windows-nt)
  (set-face-attribute 'default nil :family "Consolas"))

;; Symbola font check (required for emoji and other Unicode 6+ display)
(defvar font-symbola-p nil
  "If non-nil, Symbola font is available on the system.")

;; Check if the Symbola font is available just once, after a second delay after
;; emacs startup. This trick works when emacs is launched in regular or daemon
;; mode
(do-once-1-sec-after-emacs-startup
 (when (find-font (font-spec :name "Symbola"))
   ;; Manually choose a fallback font for Unicode
   ;; http://endlessparentheses.com/manually-choose-a-fallback-font-for-unicode.html
   (set-fontset-font "fontset-default" nil (font-spec :size 20 :name "Symbola"))
   (setq font-symbola-p t)))

(defun modi/font-size-adj (&optional arg)
  "The default C-x C-0/-/= bindings do an excellent job of font resizing.
They, though, do not change the font sizes for the text outside the buffer,
example in mode-line. Below function changes the font size in those areas too.

M-<NUM> M-x modi/font-size-adj increases font size by NUM points if NUM is +ve,
                               decreases font size by NUM points if NUM is -ve
                               resets    font size if NUM is 0."
  (interactive "p")
  (if (= arg 0)
      (setq font-size-pt default-font-size-pt)
    (setq font-size-pt (+ font-size-pt arg)))
  ;; The internal font size value is 10x the font size in points unit.
  ;; So a 10pt font size is equal to 100 in internal font size value.
  (set-face-attribute 'default nil :height (* font-size-pt 10)))

(defun modi/font-size-incr ()  (interactive) (modi/font-size-adj +1))
(defun modi/font-size-decr ()  (interactive) (modi/font-size-adj -1))
(defun modi/font-size-reset () (interactive) (modi/font-size-adj 0))

(modi/font-size-reset) ; Initialize font-size-pt var to the default value

;; Line truncation
;; Disable truncation. This setting does NOT apply to windows split using `C-x 3`
(setq-default truncate-lines nil)
;; Disable truncation in windows split using `C-x 3` too.
(setq-default truncate-partial-width-windows nil)
;; Do `M-x toggle-truncate-lines` to toggle truncation mode.

;; Visual Line Mode
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

(global-visual-line-mode -1) ; Disable visual line mode globally
(modi/turn-on-visual-line-mode) ; and then enable it only in selected modes

;; Turn on line wrapping fringe indicators in Visual Line Mode
(setq-default visual-line-fringe-indicators '(left-curly-arrow
                                              right-curly-arrow))

;; Adaptive Wrap
;; `adaptive-wrap-prefix-mode' indents the visual lines to
;; the level of the actual line plus `adaptive-wrap-extra-indent'. Thus line
;; truncation has to be off for adaptive wrap to be in effect.
(use-package adaptive-wrap
  :config
  (progn
    (setq-default adaptive-wrap-extra-indent 2)

    (defun turn-on-adaptive-wrap-prefix-mode ()
      "Turns on adaptive-wrap-prefix-mode."
      (interactive)
      (adaptive-wrap-prefix-mode 1))
    (define-globalized-minor-mode global-adaptive-wrap-prefix-mode
      adaptive-wrap-prefix-mode
      turn-on-adaptive-wrap-prefix-mode)
    (global-adaptive-wrap-prefix-mode 1)))

;; CURSOR
;; Change cursor color according to mode:
;;   read-only buffer / overwrite / regular (insert) mode
(blink-cursor-mode -1) ; Don't blink the cursor, it's distracting!

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

;; Presentation mode
(defvar prez-mode-enabled-once nil
  "Flag to indicate if prez-mode has been enabled at least once.")

(define-minor-mode prez-mode
  "Minor mode to change to light theme with bigger fonts for better readability
during presentations."
  :init-value nil
  :lighter    " Prez"
  (if prez-mode
      ;; Enable prez mode
      (progn
        (set-face-attribute 'default nil :height (* (+ 3 default-font-size-pt) 10))
        (set-frame-size (selected-frame) 80 25) ; rows and columns w h
        (delete-other-windows)
        (setq prez-mode-enabled-once t))
    ;; Disable prez mode
    (progn
      (when prez-mode-enabled-once
        (modi/font-size-reset)
        (full-screen-center)
        (split-window-right)
        (other-window 1)
        (toggle-between-buffers)))))
(defun turn-on-prez-mode ()
  "Turns on prez-mode."
  (interactive)
  (prez-mode 1))
(defun turn-off-prez-mode ()
  "Turns off prez-mode."
  (interactive)
  (prez-mode -1))
(define-globalized-minor-mode global-prez-mode prez-mode turn-on-prez-mode)

;; Hidden Mode Line Mode (Minor Mode)
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

;; Show mode line in header
;; http://bzg.fr/emacs-strip-tease.html
;; Careful: you need to deactivate hidden-mode-line-mode
(defun mode-line-in-header ()
  (interactive)
  (if (not header-line-format)
      (setq header-line-format mode-line-format)
    (setq header-line-format nil))
  (force-mode-line-update))

;; Enable / Disable Fringe
(defun enable-fringe ()
  (interactive)
  (fringe-mode '(nil . nil) ))
(defun disable-fringe ()
  (interactive)
  (fringe-mode '(0 . 0) ))

;; Generic mode to syntax highlight .vimrc files (I know, blasphemy!)
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

;; Coloring regions that have ANSI color codes in them
;; http://unix.stackexchange.com/a/19505/57923
(defun ansi-color-apply-on-region-int (beg end)
  "Colorize using the ANSI color codes."
  (interactive "r")
  (ansi-color-apply-on-region beg end))

;; Show long lines
;; http://stackoverflow.com/a/6346547/1219634
(use-package whitespace
    :commands (modi/show-long-lines)
    :init
    (progn
      (bind-to-modi-map "L" modi/show-long-lines))
    :config
    (progn
      (defun modi/show-long-lines()
        (interactive)
        (let ((hi-lock-mode -1)) ; reset hi-lock mode
          (highlight-lines-matching-regexp
           (concat ".\\{"
                   (number-to-string (+ 1 whitespace-line-column))
                   "\\}") "hi-yellow")))))

;; http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
(defun endless/narrow-or-widen-dwim (p)
  "If the buffer is narrowed, it widens. Otherwise, it narrows intelligently.
Intelligently means: region, org-src-block, org-subtree, or defun,
whichever applies first.
Narrowing to org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer is already
narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing command.
         ;; Remove this first conditional if you don't want it.
         (cond ((ignore-errors (org-edit-src-code))
                (delete-other-windows))
               ((org-at-block-p)
                (org-narrow-to-block))
               (t (org-narrow-to-subtree))))
        (t (narrow-to-defun))))

(global-unset-key (kbd "<C-down-mouse-1>"))
;; <C-down-mouse-1> is bound to `mouse-buffer-menu' by default. It is
;; inconvenient when that mouse menu pops up when I don't need it
;; to. And actually I have never used that menu :P

;; Usage: C-x - _ - 0 _ _ _ _ - - 0
;; Usage: C-x _ _ 0 - _ - _ _ _ _ - - 0
(defhydra hydra-font-resize
    (nil "C-x"
     :bind (lambda (key cmd) (bind-key key cmd modi-mode-map))
     :color red)
  "font-resize"
  ("-"   modi/font-size-decr  "Decrease")
  ("_"   modi/font-size-incr  "Increase")
  ("="   modi/font-size-incr  "Increase" :bind nil)
  ("+"   modi/font-size-incr  "Increase" :bind nil)
  ("C-0" modi/font-size-reset "Reset to default size")
  ("0"   modi/font-size-reset "Reset to default size" :bind nil)
  ("q"   nil                  "cancel" :color blue))

;; Toggle menu bar
;; Below bkp/ vars are used to restore the original frame size after disabling
;; menu bar
(defvar bkp/frame-height-pixel nil)
(defvar bkp/frame-width-pixel nil)
(defun modi/toggle-menu-bar ()
  "Toggle the menu bar. Also restore the original frame size when disabling the
menu bar."
  (interactive)
  (let ((frame-resize-pixelwise t))
    (if menu-bar-mode
        (progn ; if menu bar is visible before toggle
          (menu-bar-mode -1))
      (progn ; if menu bar is hidden before toggle
        (setq bkp/frame-height-pixel (frame-pixel-height))
        ;; `frame-pixel-width' is returning a value higher by 2 char widths in
        ;; pixels compared to that set using `set-frame-size'. So the below
        ;; adjustment has to be made.
        (setq bkp/frame-width-pixel  (- (frame-pixel-width) (* 2 (frame-char-width))))
        (menu-bar-mode 1)))
    (when (not menu-bar-mode) ; restore frame size if menu bar is hidden after toggle
      (set-frame-size (selected-frame)
                      bkp/frame-width-pixel
                      bkp/frame-height-pixel
                      :pixelwise))))

(defun modi/set-selective-display-dwim (col)
  "Call `set-selective-display' if the point is on the first column.
If point is not on the first column, pass the column number as argument to
`set-selective-display'.

If the argument COL is passed explicitly, that takes the precedence over
the above behavior."
  (interactive "P")
  (if (null col)
      (set-selective-display (current-column))
    (set-selective-display col)))
(bind-key "C-x $" #'modi/set-selective-display-dwim modi-mode-map)

(bind-keys
 :map modi-mode-map
 ("<f2>"        . modi/toggle-menu-bar)
 ;; F8 key can't be used as it launches the VNC menu
 ;; It can though be used with shift/ctrl/alt keys
 ("<S-f8>"      . prez-mode)
 ;; Make Control+mousewheel do increase/decrease font-size
 ;; http://ergoemacs.org/emacs/emacs_mouse_wheel_config.html
 ("<C-mouse-1>" . modi/font-size-reset) ; C + left mouse click
 ("<C-mouse-4>" . modi/font-size-incr) ; C + wheel-up
 ("<C-mouse-5>" . modi/font-size-decr) ; C + wheel-down
 ("C-x t"       . toggle-truncate-lines)
 ;; This line actually replaces Emacs' entire narrowing keymap.
 ("C-x n"       . endless/narrow-or-widen-dwim))

(key-chord-define-global "2w" 'menu-bar-mode) ; alternative to F2
(key-chord-define-global "8i" 'prez-mode) ; alternative to S-F8


(provide 'setup-visual)
