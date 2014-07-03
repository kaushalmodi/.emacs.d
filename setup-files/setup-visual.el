;; Time-stamp: <2014-07-03 10:44:34 kmodi>

;; Set up the looks of emacs

(setq default-font-size-pt 10 ;; default font size
      dark-theme           t  ;; initialize dark-theme var
      )

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

(defvar default-dark-theme  'zenburn)
(defvar default-light-theme 'leuven)
(defvar default-theme       'zenburn)

;; zenburn
(defun zenburn ()
  "Activate zenburn theme."
  (interactive)
  (setq dark-theme t)
  (disable-theme 'leuven)
  (load-theme 'zenburn t)
  (set-face-attribute 'fringe nil :background "#3F3F3F" :foreground "#FFFFFF")
  (when (boundp 'setup-linum-loaded)
    (set-face-attribute 'linum nil :height 0.9 :background "#3F3F3F" :foreground "#6F6F6F"))
  (when (boundp 'setup-smart-mode-line-loaded)
    (sml/apply-theme 'dark))
  )

;;leuven theme
(defun leuven ()
  "Activate leuven theme."
  (interactive)
  (setq dark-theme nil)
  (disable-theme 'zenburn)
  (load-theme 'leuven t)
  (set-face-attribute 'fringe nil :background "#F2F2F2" :foreground "#F7A421")
  (when (boundp 'setup-linum-loaded)
    (set-face-attribute 'linum nil :height 0.9 :background "#FFFFFF" :foreground "dim gray"))
  (when (boundp 'setup-smart-mode-line-loaded)
    (sml/apply-theme 'light))
  )

;; Load the theme ONLY after the frame has finished loading (needed especially
;; when running emacs in daemon mode)
;; Source: https://github.com/Bruce-Connor/smart-mode-line/issues/84#issuecomment-46429893
(add-to-list 'after-make-frame-functions
             (lambda (&rest frame)
               (funcall default-theme)))

(setq global-font-lock-mode t ;; enable font-lock or syntax highlighting globally
      font-lock-maximum-decoration t ;; use the maximum decoration level available for color highlighting
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FONT SIZE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set the font size specified in default-font-size-pt symbol
(setq font-size-pt default-font-size-pt)
;; The internal font size value is 10x the font size in points unit.
;; So a 10pt font size is equal to 100 in internal font size value.
(set-face-attribute 'default nil :height (* font-size-pt 10))

;; Below custom function are not required usually as the default C-x C-0/-/=
;; bindings do excellent job. The default binding though does not change the
;; font sizes for the text outside the buffer, example in mode-line.
;; Below functions change the font size in those areas too.

(defun font-size-incr ()
  "Increase font size by 1 pt"
  (interactive)
  (setq font-size-pt (+ font-size-pt 1))
  (set-face-attribute 'default nil :height (* font-size-pt 10)))

(defun font-size-decr ()
  "Decrease font size by 1 pt"
  (interactive)
  (setq font-size-pt (- font-size-pt 1))
  (set-face-attribute 'default nil :height (* font-size-pt 10)))

(defun font-size-reset ()
  "Reset font size to default-font-size-pt"
  (interactive)
  (setq font-size-pt default-font-size-pt)
  (set-face-attribute 'default nil :height (* font-size-pt 10)))


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
(add-hook 'sh-mode-hook      'turn-off-visual-line-mode) ;; e.g. sim.setup file
(add-hook 'org-mode-hook     'turn-on-visual-line-mode)
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
;; Frame title bar format
;; If buffer-file-name exists, show it;
;; else if you are in dired mode, show the directory name
;; else show only the buffer name (*scratch*, *Messages*, etc)
;; Append the value of PRJ_NAME env var to the above.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq frame-title-format
      (list '(buffer-file-name "%f"
                               (dired-directory dired-directory "%b"))
            " [" (getenv "PRJ_NAME") "]"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Presentation mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq presentation-mode-enabled nil)

(defun presentation-mode ()
  "Set frame size, theme and fonts suitable for presentation."
  (interactive)
  (setq font-size-pt 13)
  (set-face-attribute 'default nil :height (* font-size-pt 10))
  (set-frame-position (selected-frame) 0 0) ;; pixels x y from upper left
  (set-frame-size (selected-frame) 80 25)  ;; rows and columns w h
  (funcall default-light-theme) ;; change to default light theme
  (delete-other-windows)
  (setq presentation-mode-enabled t)
  )

(defun coding-zombie-mode ()
  "Revert to default coding mode."
  (interactive)
  (setq font-size-pt default-font-size-pt)
  (set-face-attribute 'default nil :height (* font-size-pt 10))
  (full-screen-left)
  (funcall default-theme) ;; change to default theme
  (split-window-right)
  (setq presentation-mode-enabled nil)
  )

(defun toggle-presentation-mode ()
  "Toggle between presentation and default mode."
  (interactive)
  (if presentation-mode-enabled
      (coding-zombie-mode)
    (presentation-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hidden Mode Line Mode (Minor Mode)
;; (works only when one window is open)
;; FIXME: Make this activate only if one window is open
;; See http://bzg.fr/emacs-hide-mode-line.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar-local hidden-mode-line-mode nil)

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
;; Big Fringe (Minor Mode)
;; (works only when one window is open)
;; FIXME: Make this activate only if one window is open
;; http://bzg.fr/emacs-strip-tease.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar bzg-big-fringe-mode nil)
(define-minor-mode bzg-big-fringe-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global t
  :variable bzg-big-fringe-mode
  :group 'editing-basics
  (if (not bzg-big-fringe-mode)
      (progn
        (set-fringe-style nil)
      (custom-set-faces
       '(fringe ((t (:background "#4F4F4F")))))
      (turn-on-fci-mode))
    (progn
      (set-fringe-mode
       (/ (- (frame-pixel-width)
             (* 100 (frame-char-width)))
          2))
      (custom-set-faces
       '(fringe ((t (:background "#3F3F3F")))))
      (turn-off-fci-mode)
      )))

;; ;; Now activate this global minor mode
;; (bzg-big-fringe-mode 1)

;; Use a minimal cursor
;; (setq cursor-type 'hbar)

;; ;; Get rid of the indicators in the fringe
;; (mapcar (lambda(fb) (set-fringe-bitmap-face fb 'org-hide))
;;         fringe-bitmaps)


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


(setq setup-visual-loaded t)
(provide 'setup-visual)
