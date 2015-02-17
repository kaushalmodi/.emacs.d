;; Time-stamp: <2015-02-16 16:12:23 kmodi>

;; Functions to manipulate windows and buffers

;; Source: http://www.emacswiki.org/emacs/WinnerMode
(winner-mode 1) ;; Enable winner mode
;; Winner Mode is a global minor mode. When activated, it allows to “undo”
;; (and “redo”) changes in the window configuration with the key commands
;; ‘C-c left’ and ‘C-c right’

(req-package uniquify
  :config
  (progn
    ;; The library uniquify overrides Emacs’ default mechanism for making buffer
    ;; names unique (using suffixes like <2>, <3> etc.) with a more sensible
    ;; behaviour which use parts of the file names to make the buffer names
    ;; distinguishable.
    (setq uniquify-buffer-name-style 'post-forward)))

;; Source: http://www.emacswiki.org/emacs/RecentFiles
(req-package recentf
  :require (cl)
  :config
  (progn
    (recentf-mode 1)
    (setq recentf-max-menu-items 2000)

    ;; Customizing recentf mode map
    (bind-keys
     :map recentf-dialog-mode-map
     ("/" . isearch-forward)
     ("n" . isearch-repeat-forward)
     ("N" . isearch-repeat-backward))))

(req-package windmove
  :require (key-chord)
  :config
  (progn
    (setq windmove-wrap-around t) ; default = nil
    (windmove-default-keybindings) ; Bind windmove nav to S-left/right/up/down
    (key-chord-define-global "p[" 'windmove-left)
    (key-chord-define-global "[]" 'windmove-right)))

;; Source: http://emacs.stackexchange.com/a/3334/115
;; Reopen Killed File
(defvar killed-file-list nil
  "List of recently killed files.")

(defun add-file-to-killed-file-list ()
  "If buffer is associated with a file name, add that file to the
`killed-file-list' when killing the buffer."
  (when buffer-file-name
    (push buffer-file-name killed-file-list)))

(add-hook 'kill-buffer-hook #'add-file-to-killed-file-list)

(defun reopen-killed-file ()
  "Reopen the most recently killed file, if one exists."
  (interactive)
  (if killed-file-list
      (find-file (pop killed-file-list))
    (message "No recently killed file found to reopen.")))

;; ;; Set initial frame size and position
;; ;; fills full screen of the left monitor
;; (setq initial-frame-alist
;;       '((top    . 1)
;;         (left   . 1)
;;         (width  . 235)
;;         (height . 63)))
;; ;; (add-to-list 'default-frame-alist '(left   . 0))
;; ;; (add-to-list 'default-frame-alist '(top    . 0))
;; ;; (add-to-list 'default-frame-alist '(height . 63))
;; ;; (add-to-list 'default-frame-alist '(width  . 235))

;; Source: http://www.whattheemacsd.com/
(defun toggle-window-split ()
  "Convert horizontally splitted windows to vertically splitted, and vice-versa.
Useful when you do `C-x 3` when you intended to do `C-x 2` and vice-versa."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

;; Better alternative to `toggle-window-split', `transpose-frame'
;; Converts between horz-split <-> vert-split. In addition it also rotates
;; the windows around in the frame when you have 3 or more windows.
(req-package transpose-frame
  :load-path "from-git/transpose-frame/"
  :config
  (progn
    (bind-keys
     :map modi-mode-map
     ("C-c t" . transpose-frame))))

;; Source: http://www.whattheemacsd.com/
(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

;; Source: http://www.whattheemacsd.com/
(defun delete-current-buffer-file ()
  "Deletes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to delete this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully deleted." filename)))))

;; Source: http://www.whattheemacsd.com/
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'."
                   name (file-name-nondirectory new-name)))))))

;; Display the file path of the file in current buffer and also copy it to the kill-ring
;; Source: http://camdez.com/blog/2013/11/14/emacs-show-buffer-file-name/
(defun show-copy-buffer-file-name (arg)
  "Show the full path to the current file in the minibuffer and also copy it.
Prefixed with one `universal argument', copy only the file name (not the full path).
Prefixed with two `universal argument's, copy the full path without env var replacement."
  (interactive "p")
  (let* ((file-name-full (buffer-file-name))
         file-name)
    (if file-name-full
        (progn
          (require 'cl-lib)
          (cl-case arg
            (4 (setq file-name (concat (file-name-base file-name-full) ; C-u
                                       (file-name-extension file-name-full :period))))
            (16 (setq file-name file-name-full)) ; C-u C-u
            (t (setq file-name (replace-regexp-in-string ; no prefix
                                (concat "/proj.*?_" (getenv "USER"))
                                "${PRJ_USER}"
                                file-name-full))))
          (kill-new file-name)
          (message file-name))
      (error "Buffer not visiting a file"))))

(defun reload-init ()
  "Do load-file of the emacs config file"
  (interactive)
  (load-file (concat user-emacs-directory "/init.el")))

;; Replaced the use of below with xah-run-current-file
;; (defun load-current-file ()
;;   "Load current file"
;;   (interactive)
;;   (load-file (buffer-file-name)))

;; Source: http://www.emacswiki.org/emacs-en/download/misc-cmds.el
(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer t t))

;; Revert All Buffers
(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files"
  (interactive)
  (let* ((list (buffer-list))
         (buffer (car list)))
    (while buffer
      ;; (message "test: %s %s %s %s"
      ;;          buffer
      ;;          (buffer-file-name buffer)
      ;;          (buffer-modified-p buffer)
      ;;          (file-exists-p (format "%s" (buffer-file-name buffer))))

      ;; Revert only buffers containing files which are not modified
      ;; Don't try to revert buffers like *Messages*
      (when (and (buffer-file-name buffer) (not (buffer-modified-p buffer)))
        (if (file-exists-p (format "%s" (buffer-file-name  buffer)))
            ;; if the file exists, revert the buffer
            (progn
              (set-buffer buffer)
              (revert-buffer t t t))
          ;; if the file doesn't exist, kill the buffer
          (let (kill-buffer-query-functions) ; no query done when killing buffer
            (kill-buffer buffer)
            (message "Killed buffer of non-existing file: %s" (buffer-file-name buffer)))))
      (setq list (cdr list))
      (setq buffer (car list)))
    (message "Refreshing open files")))

;; Set the frame fill the center screen
(defun full-screen-center ()
  (interactive)
  (let ((frame-resize-pixelwise t))
    (set-frame-position (selected-frame) 1910 0)   ; pixels x y from upper left
    (set-frame-size     (selected-frame) 1894 1096 :pixelwise))) ; width, height

;; Set the emacs frame/window size at startup
;; `boundp` returns t if SYMBOL's value is not void. This prevents the frame to
;; resize every time this file is re-evaluated. With the `unless boundp` condition
;; this block is evaluated only once when emacs starts.
;; (unless (boundp 'emacs-initialized)
;;   (when (window-system)
;;     (set-frame-position (selected-frame) 0 0)
;;     (set-frame-size (selected-frame) 90 30)
;;     ))

(defun modi/switch-to-scratch-and-back (arg)
  "Toggle between *scratch-MODE* buffer and the current buffer.
If a scratch buffer does not exist, create it with the major mode set to that
of the buffer from where this function is called.

    C-u COMMAND -> Open/switch to a scratch buffer in `org-mode'
C-u C-u COMMAND -> Open/switch to a scratch buffer in `emacs-elisp-mode'"
  (interactive "P")
  (if (and (null arg)
           (string-match "\*scratch" (buffer-name)))
      (switch-to-buffer (other-buffer))
    (let (mode-str)
      (cl-case (car arg)
        (4  (setq mode-str "org-mode"))
        (16 (setq mode-str "emacs-lisp-mode"))
        (t  (setq mode-str (format "%s" major-mode))))
      (switch-to-buffer (get-buffer-create
                         (concat "*scratch-" mode-str "*")))
      (modi-mode) ; Set my minor mode to activate my key bindings
      (funcall (intern mode-str))))) ; http://stackoverflow.com/a/7539787/1219634

;; Perform the "C-g" action automatically when focus moves away from the minibuffer
;; This is to avoid the irritating occassions where repeated `C-g` pressing doesn't
;; edit the mini-buffer as cursor focus has moved out of it.
;; Source: http://stackoverflow.com/questions/3022880/how-can-i-prevent-the-mini-buffer-from-displaying-previous-commands-in-emacs
(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))
(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

;; Source: http://www.emacswiki.org/emacs/SwitchingBuffers
(defun toggle-between-buffers ()
  "Toggle between 2 buffers"
  (interactive)
  (switch-to-buffer (other-buffer)))
;; (other-buffer &optional BUFFER VISIBLE-OK FRAME)
;; - Return most recently selected buffer other than BUFFER. Ignore the argument
;;   BUFFER unless it denotes a live buffer.
;; - If VISIBLE-OK==1, a buffer is returned even when it is visible in a split
;;   window.Buffers not visible in windows are preferred to visible buffers,
;;   unless optional second argument VISIBLE-OK is non-nil.
;; - If the optional third argument FRAME is non-nil, use that frame's buffer
;;   list instead of the selected frame's buffer list.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scroll without moving the point/cursor
(defun scroll-up-dont-move-point ()
  "Scroll up by 1 line without moving the point."
  (interactive)
  (scroll-up 1))

(defun scroll-down-dont-move-point ()
  "Scroll down by 1 line without moving the point."
  (interactive)
  (scroll-down 1))

(defun scroll-other-window-up-dont-move-point ()
  "Scroll other window up by 1 line without moving the point."
  (interactive)
  (scroll-other-window 1))

(defun scroll-other-window-down-dont-move-point ()
  "Scroll other window down by 1 line without moving the point."
  (interactive)
  (scroll-other-window -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Move window splitters / Resize windows
;; Source: https://github.com/abo-abo/hydra/blob/master/hydra-examples.el

(defun hydra-move-splitter-left ()
  "Move window splitter left."
  (interactive)
  (let ((windmove-wrap-around nil))
    (if (windmove-find-other-window 'right)
        (shrink-window-horizontally 1)
      (enlarge-window-horizontally 1))))

(defun hydra-move-splitter-right ()
  "Move window splitter right."
  (interactive)
  (let ((windmove-wrap-around nil))
    (if (windmove-find-other-window 'right)
        (enlarge-window-horizontally 1)
      (shrink-window-horizontally 1))))

(defun hydra-move-splitter-up ()
  "Move window splitter up."
  (interactive)
  (let ((windmove-wrap-around nil))
    (if (windmove-find-other-window 'up)
        (enlarge-window 1)
      (shrink-window 1))))

(defun hydra-move-splitter-down ()
  "Move window splitter down."
  (interactive)
  (let ((windmove-wrap-around nil))
    (if (windmove-find-other-window 'up)
        (shrink-window 1)
      (enlarge-window 1))))

(defhydra hydra-win-resize
  (nil "C-M-]" :bind (lambda (key cmd) (bind-key key cmd modi-mode-map)))
  "win-resize"
  ("]"  hydra-move-splitter-right "→")
  ("["  hydra-move-splitter-left  "←")
  ("p"  hydra-move-splitter-up    "↑") ; mnemonic: `p' for `up'
  ("\\" hydra-move-splitter-down  "↓")
  ("="  balance-windows           "Balance" :bind nil))

;; Commented out this piece of code as it is giving the below error:
;; byte-code: Wrong number of arguments: (lambda (arg)
;; (mwheel-scroll-all-function-all (quote scroll-up) arg)), 0
;; ;; Allow scrolling of all buffers using mouse-wheel in scroll-all-mode
;; ;; (by default scroll-all-mode doesn't do that)
;; ;; http://www.emacswiki.org/emacs/ScrollAllMode
;; (defun mwheel-scroll-all-function-all (func arg)
;;   (if scroll-all-mode
;;       (save-selected-window
;;         (walk-windows
;;          (lambda (win)
;;            (select-window win)
;;            (condition-case nil
;;                (funcall func arg)
;;              (error nil)))))
;;     (funcall func arg)))

;; (defun mwheel-scroll-all-scroll-up-all (arg)
;;   (mwheel-scroll-all-function-all 'scroll-up arg))

;; (defun mwheel-scroll-all-scroll-down-all (arg)
;;   (mwheel-scroll-all-function-all 'scroll-down arg))

;; (setq mwheel-scroll-up-function   'mwheel-scroll-all-scroll-up-all)
;; (setq mwheel-scroll-down-function 'mwheel-scroll-all-scroll-down-all)

(setq mwheel-scroll-up-function   'scroll-up)
(setq mwheel-scroll-down-function 'scroll-down)

;; Ediff
;; Split windows horizontally in ediff (instead of vertically)
(setq ediff-split-window-function #'split-window-horizontally)

(defun modi/set-file-permissions (perm)
  "Change permissions of the file in current buffer.
Example: M-644 M-x modi/set-file-permissions."
  (interactive "p")
  (when (<= perm 1)
    (setq perm 644))
  (let ((cmd (concat "chmod "
                     (format "%s " perm)
                     (buffer-file-name))))
    (message "%s" cmd)
    (shell-command cmd)))

(defvar modi/toggle-one-window--buffer-name nil
  "Variable to store the name of the buffer for which the `modi/toggle-one-window'
function is called.")
(defun modi/toggle-one-window ()
  "Toggles the frame state between deleting all windows other than
the current window and the windows state prior to that.

`winner' is required for this function."
  (interactive)
  (if (one-window-p)
      (progn
        (winner-undo)
        (when modi/toggle-one-window--buffer-name
          ;; This is required because `winner-undo' does not always take you
          ;; back to the same buffer; it does take you back to the same
          ;; window configuration.
          (switch-to-buffer modi/toggle-one-window--buffer-name)))
    (progn
      (setq modi/toggle-one-window--buffer-name (buffer-name))
      (delete-other-windows))))

;; Key bindings
(bind-keys
 :map modi-mode-map
 ;; overriding the `C-x C-p binding with `mark-page' command
 ("C-x 1"       . modi/toggle-one-window) ; default binding to `delete-other-windows'
 ("C-x C-p"     . show-copy-buffer-file-name)
 ("C-x C-k"     . delete-current-buffer-file)
 ("C-x C-r"     . rename-current-buffer-file)
 ("C-S-t"       . reopen-killed-file) ; same shortcut as for reopening closed tabs in browsers
 ;; Make Alt+mousewheel scroll the other buffer
 ("<M-mouse-4>" . scroll-other-window-down-dont-move-point) ; M + wheel up
 ("<M-mouse-5>" . scroll-other-window-up-dont-move-point) ; M + wheel down
 ("C-c s"       . rotate-windows)) ; rotate windows clockwise. This will do the act of swapping windows if the frame is split into only 2 windows

;; Bind a function to execute when middle clicking a buffer name in mode line
;; Source: http://stackoverflow.com/a/26629984/1219634
(bind-key "<mode-line> <mouse-2>"   'show-copy-buffer-file-name        mode-line-buffer-identification-keymap)
(bind-key "<mode-line> <S-mouse-2>" (λ (show-copy-buffer-file-name 4)) mode-line-buffer-identification-keymap)

;; Below bindings are made in global map and not in my minor mode as I want
;; other modes to override those bindings.
(unbind-key "<M-up>"    modi-mode-map)
(unbind-key "<M-down>"  modi-mode-map)
(unbind-key "<M-left>"  modi-mode-map)
(unbind-key "<M-right>" modi-mode-map)
(bind-keys
 ("<f5>"      . revert-buffer)
 ("<S-f5>"    . revert-all-buffers)
 ("<S-f9>"    . eshell)
 ("<M-up>"    . scroll-down-dont-move-point)
 ("<M-down>"  . scroll-up-dont-move-point)
 ;; Change the default `M-left` key binding from `left-word'
 ;; The same function anyways is also bound to `C-left`
 ("<M-left>"  . scroll-other-window-down-dont-move-point)
 ("<S-prior>" . scroll-other-window-down-dont-move-point) ; S-PgUp
 ;; Change the default `M-right` key binding from `right-word'
 ;; The same function anyways is also bound to `C-right`
 ("<M-right>" . scroll-other-window-up-dont-move-point)
 ("<S-next>"  . scroll-other-window-up-dont-move-point)) ; S-PgDown

(bind-to-modi-map "b" modi/switch-to-scratch-and-back)
(bind-to-modi-map "f" full-screen-center)
(bind-to-modi-map "y" bury-buffer)

(key-chord-define-global "XX" (λ (kill-buffer (current-buffer))))
(key-chord-define-global "ZZ" 'toggle-between-buffers)
(key-chord-define-global "5t" 'revert-buffer) ;; alternative to F5


(provide 'setup-windows-buffers)

;; TIPS

;; (1) `C-l'
;; C-l calls the `recenter-top-bottom' command. But typing C-l twice in a row
;; (C-l C-l) scrolls the window so that point is on the topmost screen line.
;; Typing a third C-l scrolls the window so that point is on the bottom-most
;; screen line. Each successive C-l cycles through these three positions.

;; (global-set-key (kbd "<C-tab>")   'other-window) ;; alternative shortcut for `C-x o`
;; ;; Cycle the buffers in reverse order than what happens with `C-x o`
;; (global-set-key (kbd "C-x O")
;;                 (lambda ()
;;                   (interactive)
;;                   (other-window -1)))
