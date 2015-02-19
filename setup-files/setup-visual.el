;; Time-stamp: <2015-02-19 12:08:53 kmodi>

;; Set up the looks of emacs

;; Highlight closing parentheses; show the name of the body being closed with
;; the closing parentheses in the minibuffer.
(show-paren-mode +1)

(setq default-font-size-pt 12 ; default font size
      dark-theme           t  ; initialize dark-theme var
      )

(setq frame-resize-pixelwise t) ; allow frame size to inc/dec by a pixel

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MENU/TOOL/SCROLL BARS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1)) ;; do not show the menu bar with File|Edit|Options|...
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1)) ;; do not show the tool bar with icons on the top
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)) ;; disable the scroll bars

(setq inhibit-startup-message t ;; No splash screen at startup
      scroll-step 1 ;; scroll 1 line at a time
      tooltip-mode nil ;; disable tooltip appearance on mouse hover
      )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THEME and COLORS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq default-dark-theme  'smyx)
(setq default-light-theme 'leuven)
(setq default-theme       default-dark-theme)

;; zenburn
(defun zenburn ()
  "Activate zenburn theme."
  (interactive)
  (setq dark-theme t)
  (disable-theme 'leuven)
  (disable-theme 'smyx)
  (load-theme 'zenburn t)
  (with-eval-after-load 'faces
    (modi/blend-fringe))
  (with-eval-after-load 'linum
    (modi/blend-linum))
  (with-eval-after-load 'smart-mode-line
    (sml/apply-theme 'dark))
  (with-eval-after-load 'fill-column-indicator
    (setq fci-rule-color "#383838")
    (fci-mode -1)))

;; smyx
(defun smyx ()
  "Activate smyx theme."
  (interactive)
  (setq dark-theme t)
  (disable-theme 'leuven)
  (disable-theme 'zenburn)
  (load-theme 'smyx t)
  (with-eval-after-load 'faces
    (modi/blend-fringe))
  (with-eval-after-load 'linum
    (modi/blend-linum))
  (with-eval-after-load 'smart-mode-line
    (sml/apply-theme 'dark))
  (with-eval-after-load 'fill-column-indicator
    (setq fci-rule-color "#383838")
    (fci-mode -1)))

;;leuven theme
(defun leuven ()
  "Activate leuven theme."
  (interactive)
  (setq dark-theme nil)
  (disable-theme 'zenburn)
  (disable-theme 'smyx)
  (load-theme 'leuven t)
  (with-eval-after-load 'faces
    (modi/blend-fringe))
  (with-eval-after-load 'linum
    (modi/blend-linum))
  (with-eval-after-load 'smart-mode-line
    (sml/apply-theme 'light))
  (with-eval-after-load 'fill-column-indicator
    (setq fci-rule-color "#F2F2F2")
    (fci-mode -1)))

(defun toggle-theme ()
  "Toggles theme between the default light and default dark themes."
  (interactive)
  (if dark-theme
      (funcall default-light-theme)
    (funcall default-dark-theme)))

;; Load the theme ONLY after the frame has finished loading (needed especially
;; when running emacs in daemon mode)
;; Source: https://github.com/Bruce-Connor/smart-mode-line/issues/84#issuecomment-46429893
(add-hook 'window-setup-hook (λ (funcall default-theme)))

(add-hook 'after-init-hook
          '(lambda()
             ;; Frame title bar format
             ;; If buffer-file-name exists, show it;
             ;; else if you are in dired mode, show the directory name
             ;; else show only the buffer name (*scratch*, *Messages*, etc)
             ;; Append the value of PRJ_NAME env var to the above.
             (setq frame-title-format
                   (list '(buffer-file-name "%f"
                                            (dired-directory dired-directory "%b"))
                         " [" (getenv "PRJ_NAME") "]"))))

(setq global-font-lock-mode t ;; enable font-lock or syntax highlighting globally
      font-lock-maximum-decoration t ;; use the maximum decoration level available for color highlighting
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FONTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (set-face-attribute 'default nil :font "Source Code Pro for Powerline" )
;; (set-frame-font "Input Mono" nil t)

;; Manually choose a fallback font for Unicode
;; Source: http://endlessparentheses.com/manually-choose-a-fallback-font-for-unicode.html
(set-fontset-font "fontset-default" nil
                  (font-spec :size 20 :name "Symbola"))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LINE TRUNCATION / VISUAL LINE MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Do `M-x toggle-truncate-lines` to jump in and out of truncation mode.

(setq
 ;; NOTE: Line truncation has to be enabled for the visual-line-mode to be effective
 ;; But here you see that truncation is disabled by default. It is enabled
 ;; ONLY in specific modes in which the fci-mode is enabled (setup-fci.el)
 truncate-lines nil ;; enable line wrapping. This setting does NOT apply to windows split using `C-x 3`
 truncate-partial-width-windows nil ;; enable line wrapping in windows split using `C-x 3`
 visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow) ;; Turn on line wrapping fringe indicators in Visual Line Mode
 )

;; (global-visual-line-mode 1) ;; Enable wrapping lines at word boundaries
(global-visual-line-mode -1) ;; Disable wrapping lines at word boundaries

;; Enable/Disable visual-line mode in specific major modes. Enabling visual
;; line mode does word wrapping only at word boundaries
(add-hook 'sh-mode-hook       'turn-off-visual-line-mode) ;; e.g. sim.setup file
(add-hook 'org-mode-hook      'turn-on-visual-line-mode)
(add-hook 'markdown-mode-hook 'turn-on-visual-line-mode)

(defun turn-off-visual-line-mode ()
  (interactive)
  (visual-line-mode -1))

(defun turn-on-visual-line-mode ()
  (interactive)
  (visual-line-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CURSOR
;; Change cursor color according to mode: read-only buffer / overwrite /
;; regular (insert) mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(blink-cursor-mode -1) ;; Don't blink the cursor, it's distracting!

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

(add-hook 'post-command-hook 'hcz-set-cursor-color-according-to-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Presentation mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  (prez-mode t))
(defun turn-off-prez-mode ()
  "Turns off prez-mode."
  (interactive)
  (prez-mode -1))
(define-globalized-minor-mode global-prez-mode prez-mode turn-on-prez-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hidden Mode Line Mode (Minor Mode)
;; (works only when one window is open)
;; FIXME: Make this activate only if one window is open
;; See http://bzg.fr/emacs-hide-mode-line.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Show mode line in header
;; http://bzg.fr/emacs-strip-tease.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Careful: you need to deactivate hidden-mode-line-mode
(defun mode-line-in-header ()
  (interactive)
  (if (not header-line-format)
      (setq header-line-format mode-line-format)
    (setq header-line-format nil))
  (force-mode-line-update))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable / Disable Fringe
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun enable-fringe ()
  (interactive)
  (fringe-mode '(nil . nil) ))

(defun disable-fringe ()
  (interactive)
  (fringe-mode '(0 . 0) ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic mode to syntax highlight .vimrc files (I know, blasphemy!)
;; Source: http://stackoverflow.com/questions/4236808/syntax-highlight-a-vimrc-file-in-emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(setq auto-mode-alist
      (append
       '(
         ("\\.vimrc.*\\'" . vimrc-generic-mode)
         ("\\.vim\\'" . vimrc-generic-mode)
         ) auto-mode-alist))

;; Coloring regions that have ANSI color codes in them
;; http://unix.stackexchange.com/questions/19494/how-to-colorize-text-in-emacs
(defun ansi-color-apply-on-region-int (beg end)
  "Colorize using the ANSI color codes."
  (interactive "r")
  (ansi-color-apply-on-region beg end))

;; Show long lines
;; Source: http://stackoverflow.com/questions/6344474/how-can-i-make-emacs-highlight-lines-that-go-over-80-chars
(require 'whitespace)
(defun modi/show-long-lines()
  (interactive)
  (hi-lock-mode -1) ;; reset hi-lock mode
  (highlight-lines-matching-regexp (concat ".\\{"
                                           (number-to-string (+ 1 whitespace-line-column))
                                           "\\}") "hi-yellow"))

;; Source: http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
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

;; Key bindings
(global-unset-key (kbd "<C-down-mouse-1>")) ;; it is bound to `mouse-buffer-menu'
;; by default. It is inconvenient when that mouse menu pops up when I don't need
;; it to. And actually I have never used that menu :P

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
        (menu-bar-mode +1)))
    (when (not menu-bar-mode) ; restore frame size if menu bar is hidden after toggle
      (set-frame-size (selected-frame)
                      bkp/frame-width-pixel
                      bkp/frame-height-pixel
                      :pixelwise))))

(bind-keys
 :map modi-mode-map
 ("<f2>"        . modi/toggle-menu-bar)
 ;; F8 key can't be used as it launches the VNC menu
 ;; It can though be used with shift/ctrl/alt keys
 ("<S-f8>"      . prez-mode)
 ;; Make Control+mousewheel do increase/decrease font-size
 ;; Source: http://ergoemacs.org/emacs/emacs_mouse_wheel_config.html
 ("<C-mouse-1>" . modi/font-size-reset) ; C + left mouse click
 ("<C-mouse-4>" . modi/font-size-incr) ; C + wheel-up
 ("<C-mouse-5>" . modi/font-size-decr) ; C + wheel-down
 ("C-x t"       . toggle-truncate-lines)
 ;; This line actually replaces Emacs' entire narrowing keymap.
 ("C-x n"       . endless/narrow-or-widen-dwim))

(key-chord-define-global "2w" 'menu-bar-mode) ; alternative to F2
(key-chord-define-global "8i" 'prez-mode) ; alternative to S-F8

(bind-to-modi-map "L" modi/show-long-lines)


(provide 'setup-visual)
