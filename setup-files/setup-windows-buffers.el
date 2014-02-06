;; Time-stamp: <2014-02-06 01:05:58 Kaushal>

;; Functions to manipulate windows and buffers


;; Source: http://www.emacswiki.org/emacs/WinnerMode
(winner-mode 1) ;; Enable winner mode
;; Winner Mode is a global minor mode. When activated, it allows to “undo”
;; (and “redo”) changes in the window configuration with the key commands
;; ‘C-c left’ and ‘C-c right’

(require 'uniquify)
;; The library uniquify overrides Emacs’ default mechanism for making buffer
;; names unique (using suffixes like <2>, <3> etc.) with a more sensible
;; behaviour which use parts of the file names to make the buffer names
;; distinguishable.
(setq uniquify-buffer-name-style 'post-forward)

;; Source: http://www.emacswiki.org/emacs/RecentFiles
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 50)

;; Set initial frame size and position
;; fills full screen of the left monitor
(setq initial-frame-alist
      '((top    . 1)
        (left   . 1)
        (width  . 235)
        (height . 63)))
;; (add-to-list 'default-frame-alist '(left   . 0))
;; (add-to-list 'default-frame-alist '(top    . 0))
;; (add-to-list 'default-frame-alist '(height . 63))
;; (add-to-list 'default-frame-alist '(width  . 235))

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
(defun show-copy-buffer-file-name ()
  "Show the full path to the current file in the minibuffer and also copy it."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if file-name
        (progn
          (message file-name)
          (kill-new file-name))
      (error "Buffer not visiting a file"))))

(defun reload-init ()
  "Do load-file of the emacs config file"
  (interactive)
  (load-file (concat user-emacs-directory "/init.el")))

(defun load-current-file ()
  "Load current file"
  (interactive)
  (load-file (buffer-file-name)))

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
       (when (and (buffer-file-name buffer) (not (buffer-modified-p buffer)))
         (set-buffer buffer)
         (revert-buffer t t t))
       (setq list (cdr list))
       (setq buffer (car list))))
  (message "Refreshing open files"))

;; Set the frame size to fill the left screen
(defun full-screen-left ()
  (interactive)
  (set-frame-position (selected-frame) 0 0) ;; pixels x y from upper left
  (set-frame-size (selected-frame) 235 63)  ;; rows and columns w h
  )

;; Set the emacs frame/window size at startup
;; `boundp` returns t if SYMBOL's value is not void. This prevents the frame to
;; resize every time this file is re-evaluated. With the `unless boundp` condition
;; this block is evaluated only once when emacs starts.
;; (unless (boundp 'emacs-initialized)
;;   (when (window-system)
;;     (set-frame-position (selected-frame) 0 0)
;;     (set-frame-size (selected-frame) 90 30)
;;     ))

;; Source: http://www.emacswiki.org/emacs/RecreateScratchBuffer
(defun switch-to-scratch-and-back ()
  "Toggle between *scratch* buffer and the current buffer.
     If the *scratch* buffer does not exist, create it."
  (interactive)
  (let ((scratch-buffer-name (get-buffer-create "*scratch*")))
    (if (equal (current-buffer) scratch-buffer-name)
        (switch-to-buffer (other-buffer))
      (switch-to-buffer scratch-buffer-name
                        ;; (lisp-interaction-mode)
                        ))))

;; TODO: Fix below function, it sort of works, not perfect
;; Reopen the last killed buffer
;; Source: http://stackoverflow.com/questions/10394213/emacs-reopen-previous-killed-buffer
(require 'cl)
(require 'recentf)
(defun undo-kill-buffer ()
  (interactive)
  (let ((active-files (loop for buf in (buffer-list)
                            when (buffer-file-name buf) collect it)))
    (loop for file in recentf-list
          unless (member file active-files) return (find-file file))))

;; Perform the "C-g" action automatically when focus moves away from the minibuffer
;; This is to avoid the irritating occassions where repeated `C-g` pressing doesn't
;; edit the mini-buffer as cursor focus has moved out of it.
;; Source: http://stackoverflow.com/questions/3022880/how-can-i-prevent-the-mini-buffer-from-displaying-previous-commands-in-emacs
(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))
(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)


;; Source: http://www.emacswiki.org/emacs/FullScreen
(defun toggle-fullscreen ()
  "Toggle full screen on X11"
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))


;; Source: http://www.emacswiki.org/emacs/SwitchingBuffers
(defun toggle-between-buffers ()
  "Toggle between 2 buffers"
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))



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


(setq setup-windows-buffers-loaded t)
(provide 'setup-windows-buffers)

;; TIPS

;; (1) `C-l'
;; C-l calls the `recenter-top-bottom' command. But typing C-l twice in a row
;; (C-l C-l) scrolls the window so that point is on the topmost screen line.
;; Typing a third C-l scrolls the window so that point is on the bottom-most
;; screen line. Each successive C-l cycles through these three positions.
