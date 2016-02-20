;; Time-stamp: <2016-02-20 01:40:39 kmodi>

;; Functions to manipulate windows and buffers

;; When multiple buffers are visible (like in a frame with 2 or more windows),
;; do not display an already visible buffer when switching to next/previous
;; buffers or after killing buffers.
(setq switch-to-visible-buffer nil)

;; http://www.emacswiki.org/emacs/WinnerMode
;; Winner Mode is a global minor mode. When activated, it allows to “undo”
;; (and “redo”) changes in the window configuration with the key commands
;; ‘C-c left’ and ‘C-c right’
(use-package winner
  :config
  (progn
    (winner-mode 1)))

;; Uniquify
;; The library uniquify overrides Emacs’ default mechanism for making buffer
;; names unique (using suffixes like <2>, <3> etc.) with a more sensible
;; behaviour which use parts of the file names to make the buffer names
;; distinguishable.
(use-package uniquify
  :config
  (progn
    (setq uniquify-buffer-name-style 'post-forward)))

;; http://www.emacswiki.org/emacs/RecentFiles
(use-package recentf
  :config
  (progn
    (recentf-mode 1)
    (setq recentf-max-menu-items 2000)))

(use-package windmove
  ;; Bind in the global map so that the below S-arrow bindings can be
  ;; overridden in org-mode
  :bind (("S-<left>"  . windmove-left)
         ("S-<right>" . windmove-right)
         ("S-<up>"    . windmove-up)
         ("S-<down>"  . windmove-down))
  :bind (:map modi-mode-map
         ("C-c ]" . hydra-win-resize/body)
         ("C-c [" . hydra-win-resize/body))
  :config
  (progn
    (setq windmove-wrap-around t) ; default = nil

    ;; Move window splitters / Resize windows
    ;; https://github.com/abo-abo/hydra/blob/master/hydra-examples.el
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

    (defhydra hydra-win-resize (:color red)
      "win-resize"
      ("]"        hydra-move-splitter-right "→")
      ("["        hydra-move-splitter-left  "←")
      ("p"        hydra-move-splitter-up    "↑") ; mnemonic: `p' for `up'
      ("{"        hydra-move-splitter-up    "↑")
      ("\\"        hydra-move-splitter-down  "↓")
      ("}"        hydra-move-splitter-down  "↓")
      ("="        balance-windows           "Balance")
      ("q"        nil "cancel" :color blue)
      ("<return>" nil "cancel" :color blue))))

;; Reopen Killed File
;; http://emacs.stackexchange.com/a/3334/115
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

;; Transpose Frame
;; http://www.emacswiki.org/emacs/transpose-frame.el
(use-package transpose-frame
  :bind (:map modi-mode-map
         ("C-c o"   . rotate-frame)
         ("C-c C-\\" . transpose-frame))) ; toggles between horz/vert splits

(defun modi/delete-current-buffer-file ()
  "Deletes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when (and filename
               (file-exists-p filename)
               (yes-or-no-p "Are you sure you want to delete this file? "))
      (delete-file filename)
      (message "File `%s' successfully deleted." filename))
    (kill-buffer (current-buffer))))

;; http://www.whattheemacsd.com/
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer `%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named `%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File `%s' successfully renamed to `%s'."
                   name (file-name-nondirectory new-name)))))))

;; Display the file path of the file in current buffer and also copy it to
;; the kill-ring
;; http://camdez.com/blog/2013/11/14/emacs-show-buffer-file-name/
(defun show-copy-buffer-file-name (arg)
  "Show the full path to the current file in the minibuffer and also copy it.

    C-u COMMAND -> Copy only the file name (not the full path).
C-u C-u COMMAND -> Copy the full path without env var replacement."
  (interactive "p")
  (let* ((file-name-full (buffer-file-name))
         file-name)
    (if file-name-full
        (progn
          (cl-case arg
            (4 (setq file-name (concat (file-name-base file-name-full) ; C-u
                                       (file-name-extension file-name-full :period))))
            (16 (setq file-name file-name-full)) ; C-u C-u
            (t (setq file-name (replace-regexp-in-string ; no prefix
                                (concat "_" (getenv "USER")) "_${USER}"
                                file-name-full))))
          (kill-new file-name)
          (message file-name))
      (error "Buffer not visiting a file"))))

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
    (set-frame-position nil 1910 0) ; pixels x y from upper left
    (set-frame-size     nil 1894 1096 :pixelwise))) ; width, height

;; http://emacs.stackexchange.com/a/81/115
(defun modi/switch-to-scratch-and-back (arg)
  "Toggle between *scratch-MODE* buffer and the current buffer.
If a scratch buffer does not exist, create it with the major mode set to that
of the buffer from where this function is called.

        COMMAND -> Open/switch to a scratch buffer in the current buffer's major mode
    C-0 COMMAND -> Open/switch to a scratch buffer in `fundamental-mode'
    C-u COMMAND -> Open/switch to a scratch buffer in `org-mode'
C-u C-u COMMAND -> Open/switch to a scratch buffer in `emacs-elisp-mode'"
  (interactive "p")
  (if (and (= arg 1) ; no prefix
           (string-match-p "\\*scratch" (buffer-name)))
      (switch-to-buffer (other-buffer))
    (let ((mode-str (cl-case arg
                      (0  "fundamental-mode") ; C-0
                      (4  "org-mode") ; C-u
                      (16 "emacs-lisp-mode") ; C-u C-u
                      (t  (format "%s" major-mode))))) ; no prefix
      (switch-to-buffer (get-buffer-create
                         (concat "*scratch-" mode-str "*")))
      (funcall (intern mode-str))))) ; http://stackoverflow.com/a/7539787/1219634

;; Perform the "C-g" action automatically when focus moves away from the minibuffer
;; This is to avoid the irritating occassions where repeated `C-g` pressing doesn't
;; edit the mini-buffer as cursor focus has moved out of it.
;; http://stackoverflow.com/a/3024055/1219634
(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))
(add-hook 'mouse-leave-buffer-hook #'stop-using-minibuffer)

;; http://www.emacswiki.org/emacs/SwitchingBuffers
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

;; Keep point at its screen position if the scroll command moved it vertically
;; out of the window, e.g. when scrolling by full screens using C-v.
(setq scroll-preserve-screen-position t)

;; Scroll without moving the point/cursor
(defun scroll-up-dont-move-point ()
  "Scroll up by 1 line without moving the point."
  (interactive) (scroll-up 1))

(defun scroll-down-dont-move-point ()
  "Scroll down by 1 line without moving the point."
  (interactive) (scroll-down 1))

(defun scroll-other-window-up-dont-move-point ()
  "Scroll other window up by 1 line without moving the point."
  (interactive) (scroll-other-window 1))

(defun scroll-other-window-down-dont-move-point ()
  "Scroll other window down by 1 line without moving the point."
  (interactive) (scroll-other-window -1))

;; Below bindings are made in global map and not in my minor mode as I want
;; other modes to override those bindings.
(bind-keys
 ("<C-M-up>"    . scroll-down-dont-move-point)
 ("<C-M-down>"  . scroll-up-dont-move-point)
 ("<C-M-left>"  . scroll-other-window-down-dont-move-point)
 ("<C-M-right>" . scroll-other-window-up-dont-move-point))

(bind-keys
 :map modi-mode-map
  ;; Make Alt+mousewheel scroll the other buffer
  ("<M-mouse-4>" . scroll-other-window-down-dont-move-point) ; M + wheel up
  ("<M-mouse-5>" . scroll-other-window-up-dont-move-point)) ; M + wheel down

;; Allow scrolling of all buffers using mouse-wheel in `scroll-all-mode'.
;; By default, `scroll-all-mode' works only with C-v/M-v.
(defun modi/advice-mwhell-scroll-all (orig-fun &rest args)
  "Execute ORIG-FUN in all the windows."
  (let (ret)
    (if scroll-all-mode
        (save-selected-window (walk-windows (lambda (win)
                                              (select-window win)
                                              (condition-case nil
                                                  (setq ret (apply orig-fun args))
                                                (error nil)))))
      (setq ret (apply orig-fun args)))
    ret))
(advice-add 'scroll-up   :around #'modi/advice-mwhell-scroll-all)
(advice-add 'scroll-down :around #'modi/advice-mwhell-scroll-all)

;; Ediff
(use-package ediff
  :commands (ediff-files ediff-buffers modi/ediff-dwim)
  :config
  (progn
    ;; No separate frame for ediff control buffer
    (setq ediff-window-setup-function #'ediff-setup-windows-plain)

    ;; Split windows horizontally in ediff (instead of vertically)
    (setq ediff-split-window-function #'split-window-horizontally)

    (defun modi/ediff-dwim ()
      "Do ediff as I mean.

- If a region is active, call `ediff-regions-wordwise'.
- Else if the frame has 2 windows with identical major modes,
  - Do `ediff-files' if the buffers are associated to files and the buffers
    have not been modified.
  - Do `ediff-buffers' otherwise.
- Else if the current is a file buffer with a VC backend, call `vc-ediff'
- Else call `ediff-buffers'."
      (interactive)
      (let* ((num-win (safe-length (window-list)))
             (bufa (get-buffer (buffer-name)))
             (filea (buffer-file-name bufa))
             (modea (with-current-buffer bufa major-mode))
             bufb fileb modeb)
        (save-excursion
          (other-window 1)
          (setq bufb (get-buffer (buffer-name)))
          (setq fileb (buffer-file-name bufb))
          (setq modeb (with-current-buffer bufb major-mode)))
        (cond
         ;; If a region is selected
         ((region-active-p)
          (call-interactively 'ediff-regions-wordwise))
         ;; Else If 2 windows with same major modes
         ((and (= 2 num-win)
               (eq modea modeb))
          (if (or
               ;; if either of the buffers is not associated to a file
               (null filea) (null fileb)
               ;; if either of the buffers is modified
               (buffer-modified-p bufa) (buffer-modified-p bufb))
              (progn
                (message "Running (ediff-buffers \"%s\" \"%s\") .." bufa bufb)
                (ediff-buffers bufa bufb))
            (progn
              (message "Running (ediff-files \"%s\" \"%s\") .." filea fileb)
              (ediff-files filea fileb))))
         ;; Else If file in current buffer has a vc backend
         ((and (buffer-file-name)
               (vc-registered (buffer-file-name)))
          (call-interactively 'vc-ediff))
         ;; Else call `ediff-buffers'
         (t (call-interactively 'ediff-buffers)))))))

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
    (shell-command cmd "*Shell Temp*")
    (kill-buffer "*Shell Temp*")))

(defvar modi/toggle-one-window--buffer-name nil
  "Variable to store the name of the buffer for which the `modi/toggle-one-window'
function is called.")
(defvar modi/toggle-one-window--window-configuration nil
  "Variable to store the window configuration before `modi/toggle-one-window'
function was called.")
(defun modi/toggle-one-window (force-one-window)
  "Toggles the frame state between deleting all windows other than
the current window and the windows state prior to that."
  (interactive "P")
  (if (or (null (one-window-p))
          force-one-window)
      (progn
        (setq modi/toggle-one-window--buffer-name (buffer-name))
        (setq modi/toggle-one-window--window-configuration (current-window-configuration))
        (delete-other-windows))
    (progn
      (when modi/toggle-one-window--buffer-name
        (set-window-configuration modi/toggle-one-window--window-configuration)
        (switch-to-buffer modi/toggle-one-window--buffer-name)))))

;; https://tsdh.wordpress.com/2015/03/03/swapping-emacs-windows-using-dragndrop/
(defun th/swap-window-buffers-by-dnd (drag-event)
  "Swaps the buffers displayed in the DRAG-EVENT's start and end window."
  (interactive "e")
  (let ((start-win (cl-caadr drag-event))
        (end-win   (cl-caaddr drag-event)))
    (when (and (windowp start-win)
               (windowp end-win)
               (not (eq start-win end-win))
               (not (memq (minibuffer-window)
                          (list start-win end-win))))
      (let ((bs (window-buffer start-win))
            (be (window-buffer end-win)))
        (unless (eq bs be)
          (set-window-buffer start-win be)
          (set-window-buffer end-win bs))))))
(bind-key "<C-S-drag-mouse-1>" #'th/swap-window-buffers-by-dnd modi-mode-map)

(defun modi/kill-buffer-dwim (kill-next-error-buffer)
  "Kill the current buffer.

If KILL-NEXT-ERROR-BUFFER is non-nil, kill the `next-error' buffer. Examples of such
buffers: *gtags-global*, *ag*, *Occur*."
  (interactive "P")
  (if kill-next-error-buffer
      (kill-buffer (next-error-find-buffer))
    (kill-buffer (current-buffer))))

(defun modi/quit-and-kill-window ()
  "Quit window and kill instead of burying the buffer in it."
  (interactive)
  (quit-window :kill))

;; Update bindings in few read-only modes
;; http://stackoverflow.com/a/27091776/1219634
;; Cannot set below to `'(map1 map2)'; it has to be `(list map1 map2)'.
(defconst modi/read-only-mode-maps (list special-mode-map
                                         tabulated-list-mode-map)
  "List of read-only mode maps in which few key bindings need to be updated.")
(dolist (map modi/read-only-mode-maps)
  (define-key map (kbd "y") #'bury-buffer) ; only bury
  (define-key map (kbd "k") #'modi/kill-buffer-dwim) ; only kill
  (define-key map (kbd "z") #'quit-window) ; quit + bury
  (define-key map (kbd "q") #'modi/quit-and-kill-window)) ; quit + kill

(bind-keys
 :map modi-mode-map
  ("C-x 1"        . modi/toggle-one-window) ; default binding to `delete-other-windows'
  ;; overriding `C-x C-p' originally bound to `mark-page'
  ("C-x C-p"      . show-copy-buffer-file-name)
  ;; overriding `C-x <delete>' originally bound to `backward-kill-sentence'
  ("C-x <delete>" . modi/delete-current-buffer-file)
  ("C-x C-r"      . rename-current-buffer-file)
  ("C-x O"        . other-window)
  ("C-S-t"        . reopen-killed-file) ; mimick reopen-closed-tab in browsers
  ("C-("          . toggle-between-buffers)
  ("C-)"          . modi/kill-buffer-dwim))

;; Bind a function to execute when middle clicking a buffer name in mode line
;; http://stackoverflow.com/a/26629984/1219634
(bind-key "<mode-line> <mouse-2>" #'show-copy-buffer-file-name
          mode-line-buffer-identification-keymap)
(bind-key "<mode-line> <S-mouse-2>" (lambda ()
                                      (interactive)
                                      (show-copy-buffer-file-name 4))
          mode-line-buffer-identification-keymap)

;; Below bindings are made in global map as I want them to work even when my
;; minor mode is disabled
(bind-keys
 ("<f5>"   . revert-buffer)
 ("<S-f5>" . revert-all-buffers)
 ("<S-f9>" . eshell))

(bind-to-modi-map "b" #'modi/switch-to-scratch-and-back)
(bind-to-modi-map "f" #'full-screen-center)
(bind-to-modi-map "y" #'bury-buffer)

(key-chord-define-global "XX" #'modi/kill-buffer-dwim)
(key-chord-define-global "ZZ" #'toggle-between-buffers)
(key-chord-define-global "5t" #'revert-buffer) ; alternative to F5


(provide 'setup-windows-buffers)

;; TIPS

;; (1) `C-l'
;; C-l calls the `recenter-top-bottom' command. But typing C-l twice in a row
;; (C-l C-l) scrolls the window so that point is on the topmost screen line.
;; Typing a third C-l scrolls the window so that point is on the bottom-most
;; screen line. Each successive C-l cycles through these three positions.
