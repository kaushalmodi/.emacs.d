;; Time-stamp: <2017-05-11 18:12:42 kmodi>

;; Windows and buffers manipulation

;; Contents:
;;
;;  Variables
;;  Winner Mode
;;  Uniquify
;;  Recentf
;;  Windmove
;;  Reopen Killed File
;;  Transpose Frame
;;  Current File Buffer Actions
;;  Revert buffer
;;  Frame setup
;;  Scratch-and-Back
;;  Minibuffer and Recursive Edit
;;  Untouchable Minibuffer Prompt
;;  Toggle between buffers
;;  Scrolling
;;  File Permissions
;;  One Window Toggle
;;  Kill/Bury Buffer
;;  Other Window/Buffer
;;  *Messages* Auto-tail
;;  Bindings
;;    Read-only Buffer Bindings
;;    Other Bindings

;;; Variables
;; When multiple buffers are visible (like in a frame with 2 or more windows),
;; do not display an already visible buffer when switching to next/previous
;; buffers or after killing buffers.
(setq switch-to-visible-buffer nil)

(setq recenter-positions '(0.50 0.07 0.93)) ;default: '(middle top bottom)
;; First C-l  -> 0.50: Put point vertically at the middle of the window
;; Second C-l -> 0.07: Put point close to the top of the window. If
;;                     (window-height) returns 70, that's roughly 4 lines.
;; Third C-l  -> 0.93: Put point close to the bottom of the window ~ 3 lines.
;; With the default values of `recenter-positions' and `scroll-margin' (0),
;; the "top" position is the first line of the window, and the "bottom"
;; position is the last line. Above settings provides a margin of 3 or 4 lines
;; for my default window size for the "top" and "bottom" iterations.

;;; Winner Mode
;; http://www.emacswiki.org/emacs/WinnerMode
;; Winner Mode is a global minor mode. When activated, it allows to “undo”
;; (and “redo”) changes in the window configuration with the key commands
;; ‘C-c left’ and ‘C-c right’
(use-package winner
  :config
  (progn
    (winner-mode 1)))

;;; Uniquify
;; The library uniquify overrides Emacs’ default mechanism for making buffer
;; names unique (using suffixes like <2>, <3> etc.) with a more sensible
;; behaviour which use parts of the file names to make the buffer names
;; distinguishable.
(use-package uniquify
  :config
  (progn
    (setq uniquify-buffer-name-style 'post-forward)))

;;; Recentf
;; http://www.emacswiki.org/emacs/RecentFiles
(use-package recentf
  :defer 1
  :config
  (progn
    (recentf-mode 1)
    (setq recentf-max-saved-items 200)))

;;; Windmove
(use-package windmove
  :bind (:map modi-mode-map
         ("s-<left>"  . windmove-left)
         ("s-<right>" . windmove-right)
         ("s-<up>"    . windmove-up)
         ("s-<down>"  . windmove-down)
         ("C-c ]"     . hydra-win-resize/body)
         ("C-c ["     . hydra-win-resize/body))
  :config
  (progn
    (setq windmove-wrap-around t) ; default = nil

    ;; Move window splitters / Resize windows
    ;; https://github.com/abo-abo/hydra/blob/master/hydra-examples.el
    (defun hydra-move-splitter-left (delta)
      "Move window splitter left."
      (interactive "p")
      (let ((windmove-wrap-around nil))
        (if (windmove-find-other-window 'right)
            (shrink-window-horizontally delta)
          (enlarge-window-horizontally delta))))

    (defun hydra-move-splitter-right (delta)
      "Move window splitter right."
      (interactive "p")
      (let ((windmove-wrap-around nil))
        (if (windmove-find-other-window 'right)
            (enlarge-window-horizontally delta)
          (shrink-window-horizontally delta))))

    (defun hydra-move-splitter-up (delta)
      "Move window splitter up."
      (interactive "p")
      (let ((windmove-wrap-around nil))
        (if (windmove-find-other-window 'up)
            (enlarge-window delta)
          (shrink-window delta))))

    (defun hydra-move-splitter-down (delta)
      "Move window splitter down."
      (interactive "p")
      (let ((windmove-wrap-around nil))
        (if (windmove-find-other-window 'up)
            (shrink-window delta)
          (enlarge-window delta))))

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

;;; Reopen Killed File
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

;;; Transpose Frame
;; http://www.emacswiki.org/emacs/transpose-frame.el
(use-package transpose-frame
  :bind (:map modi-mode-map
         ("C-c o"   . rotate-frame)
         ("C-c C-\\" . transpose-frame))) ; toggles between horz/vert splits

;;; Current File Buffer Actions
;; Delete current buffer file
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

;; Rename current buffer file
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
(defun modi/copy-buffer-file-name (arg)
  "Show the full path to the current file in the minibuffer and also copy it.

If the full file path has a sub-string \"_xyz\" where xyz is the user name,
replace that with \"_${USER}\".

    C-u COMMAND -> Copy only the file name (not the full path).
C-u C-u COMMAND -> Copy the full path without env var replacement."
  (interactive "p")
  (let* ((file-name-full (buffer-file-name))
         (file-name (when file-name-full
                      (cl-case arg
                        (4 (file-name-nondirectory file-name-full)) ; C-u
                        (16 file-name-full) ; C-u C-u
                        (t ; If $USER==xyz, replace _xyz with _${USER} in file name
                         (replace-regexp-in-string ; no prefix
                          (concat "_" (getenv "USER")) "_$USER" file-name-full))))))
    (if file-name
        (progn
          (kill-new file-name)
          (message "Copied file name `%s'" file-name))
      (error "Buffer not visiting a file"))))

;;; Revert buffer
(defun modi/revert-all-file-buffers ()
  "Refresh all open file buffers without confirmation.
Buffers in modified (not yet saved) state in emacs will not be reverted. They
will be reverted though if they were modified outside emacs.
Buffers visiting files which do not exist any more or are no longer readable
will be killed."
  (interactive)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      ;; (message "buf:%s  filename:%s  modified:%s  filereadable:%s"
      ;;          buf filename
      ;;          (buffer-modified-p buf) (file-readable-p (format "%s" filename)))

      ;; Revert only buffers containing files, which are not modified;
      ;; do not try to revert non-file buffers like *Messages*.
      (when (and filename
                 (not (buffer-modified-p buf)))
        (if (file-readable-p filename)
            ;; If the file exists and is readable, revert the buffer.
            (with-current-buffer buf
              (revert-buffer :ignore-auto :noconfirm :preserve-modes))
          ;; Otherwise, kill the buffer.
          (let (kill-buffer-query-functions) ; No query done when killing buffer
            (kill-buffer buf)
            (message "Killed non-existing/unreadable file buffer: %s" filename))))))
  (message "Finished reverting buffers containing unmodified files."))

(defun modi/revert-noconfirm-help-buffers (&rest args)
  "Don't confirm when reverting *Help* buffers."
  (list (car args) :noconfirm))
(advice-add 'help-mode-revert-buffer :filter-args #'modi/revert-noconfirm-help-buffers)

;;; Frame setup
(defun modi/frame-setup-1 ()
  "Set the frame to fill the center screen."
  (interactive)
  (let ((frame-resize-pixelwise t)) ; do not round frame sizes to character h/w
    (set-frame-position nil 2560 0) ; pixels x y from upper left
    (set-frame-size nil 2540 1347 :pixelwise))) ; width, height

(defun modi/frame-width-2x (double)
  "Set the frame text width to half the current width.
If DOUBLE is non-nil, the frame text width is doubled. "
  (interactive "P")
  (let ((frame-resize-pixelwise t) ; do not round frame sizes to character h/w
        (factor (if double 2 0.5)))
    (set-frame-size nil
                    (round (* factor (frame-text-width))) (frame-text-height)
                    :pixelwise)))

;;; Scratch-and-Back
;; http://emacs.stackexchange.com/a/81/115
(defun modi/switch-to-scratch-and-back (&optional arg)
  "Toggle between *scratch-MODE* buffer and the current buffer.
If a scratch buffer does not exist, create it with the major mode set to that
of the buffer from where this function is called.

        COMMAND -> Open/switch to a scratch buffer in the current buffer's major mode
    C-0 COMMAND -> Open/switch to a scratch buffer in `fundamental-mode'
    C-u COMMAND -> Open/switch to a scratch buffer in `org-mode'
C-u C-u COMMAND -> Open/switch to a scratch buffer in `emacs-elisp-mode'

Even if the current major mode is a read-only mode (derived from `special-mode'
or `dired-mode'), we would want to be able to write in the scratch buffer. So
the scratch major mode is set to `org-mode' for such cases.

Return the scratch buffer opened."
  (interactive "p")
  (if (and (or (null arg)               ; no prefix
               (= arg 1))
           (string-match-p "\\*scratch" (buffer-name)))
      (switch-to-buffer (other-buffer))
    (let* ((mode-str (cl-case arg
                       (0  "fundamental-mode") ; C-0
                       (4  "org-mode") ; C-u
                       (16 "emacs-lisp-mode") ; C-u C-u
                       ;; If the major mode turns out to be a `special-mode'
                       ;; derived mode, a read-only mode like `help-mode', open
                       ;; an `org-mode' scratch buffer instead.
                       (t (if (or (derived-mode-p 'special-mode) ; no prefix
                                  (derived-mode-p 'dired-mode))
                              "org-mode"
                            (format "%s" major-mode)))))
           (buf (get-buffer-create (concat "*scratch-" mode-str "*"))))
      (switch-to-buffer buf)
      (funcall (intern mode-str))   ; http://stackoverflow.com/a/7539787/1219634
      buf)))

;;; Minibuffer and Recursive Edit
;; Quit the minibuffer automatically when focus moves away from it (which could
;; have happened by actions like clicking some other buffer using the mouse or
;; by hitting `C-x o'). This is to avoid the irritating occasions where repeated
;; `C-g' pressing doesn't kill the minibuffer prompt as emacs has entered a
;; recursive edit session.
;; http://stackoverflow.com/a/3024055/1219634
;; The right way to exit a recursive edit session is by hitting `C-]', which is
;; bound to `abort-recursive-edit' by default.
(defun abort-recursive-edit-in-minibuffer ()
  "Disable recursive edit in minibuffer if `disable-recursive-edit-in-minibuffer'
is set to a non-nil value."
  (when (and (bound-and-true-p disable-recursive-edit-in-minibuffer)
             (active-minibuffer-window)
             (>= (recursion-depth) 1))
    (abort-recursive-edit)))
(add-hook 'mouse-leave-buffer-hook #'abort-recursive-edit-in-minibuffer)

;; http://oremacs.com/2016/06/06/counsel-set-variable/
(when (not (bound-and-true-p disable-recursive-edit-in-minibuffer))
  ;; Allow to read from minibuffer while in minibuffer.
  (setq enable-recursive-minibuffers t)
  ;; Show the minibuffer depth (when larger than 1)
  (minibuffer-depth-indicate-mode 1))

;;; Untouchable Minibuffer Prompt
;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=21874
;; Do not allow the cursor to go over or select the minibuffer prompt.
;; A good example is that we wouldn't want to ever edit/select the "Find file:"
;; prompt we see in the minibuffer when we do `find-file'.
(>=e "25.0"
    (let (;; (get ..)                   -> ((quote (read-only t face minibuffer-prompt)))
          ;; (car (get ..))             -> (quote (read-only t face minibuffer-prompt))
          ;; (eval (car (get ..)))      -> (read-only t face minibuffer-prompt)
          ;; http://thread.gmane.org/gmane.emacs.devel/202463/focus=202496
          (default (eval (car (get 'minibuffer-prompt-properties 'standard-value))))
          (dont-touch-prompt-prop '(cursor-intangible t)))
      (setq minibuffer-prompt-properties (append default dont-touch-prompt-prop))
      ;; Note: If the above `minibuffer-prompt-properties' is set using the
      ;; Customize interface, `cursor-intangible-mode' would be automatically
      ;; added to `minibuffer-setup-hook' because of the presence of
      ;; `cursor-intangible' property in `minibuffer-prompt-properties'.
      ;; (see cus-start.el).
      (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)))

;;; Toggle between buffers
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

;;; Scrolling
;; Keep point at its screen position if the scroll command moved it vertically
;; out of the window, e.g. when scrolling by full screens using C-v.
(setq scroll-preserve-screen-position t)

;; Scroll without moving the point/cursor
(defun modi/scroll-up (ln)
  "Scroll up by LN lines without moving the point.
If LN is nil, defaults to 1 line."
  (interactive "p")
  (scroll-up ln))

(defun modi/scroll-down (ln)
  "Scroll down by LN lines without moving the point.
If LN is nil, defaults to 1 line."
  (interactive "p")
  (scroll-down ln))

;; https://github.com/politza/pdf-tools/issues/227#issuecomment-242100968
(defun modi/scroll-other-window (ln)
  "Scroll the buffer in other window.

This command supports pdf file buffers too (`pdf-view-mode').

If LN is positive, scroll the buffer up.
If LN is negative, scroll the buffer down."
  (interactive "p")
  (let ((other-win (other-window-for-scrolling)))
    (if (and (fboundp #'pdf-util-pdf-window-p)
             (pdf-util-pdf-window-p other-win))
        (with-current-buffer (window-buffer other-win)
          (with-selected-window other-win
            (if (>= ln 1)
                (pdf-view-next-line-or-next-page ln)
              (pdf-view-previous-line-or-previous-page (- ln))))
          (set-window-point other-win (point)))
      (if (modi/mouse-scroll-p last-input-event) ;defined in `setup-mouse.el'
          ;; If using mouse to scroll the other window, respect the scroll
          ;; amount set in `mouse-wheel-scroll-amount'.
          (let* ((mouse-ln-1 (car mouse-wheel-scroll-amount))
                 (mouse-ln (if (natnump ln)
                               mouse-ln-1 ;scroll up
                             (- mouse-ln-1)))) ;scroll down
            (scroll-other-window mouse-ln))
        (scroll-other-window ln)))))

(defalias 'modi/scroll-other-window-up 'modi/scroll-other-window)

(defun modi/scroll-other-window-down (ln)
  "Scroll other window down by LN lines without moving the point.
If LN is nil, defaults to 1 line."
  (interactive "p")
  (modi/scroll-other-window (- ln)))

;; Below bindings are made in global map and not in my minor mode as I want
;; to allow other modes to override these.
(bind-keys
 ("<C-M-up>"    . modi/scroll-down)
 ("<C-M-down>"  . modi/scroll-up)
 ("<C-M-left>"  . modi/scroll-other-window-down)
 ("<C-M-right>" . modi/scroll-other-window-up))

;;; File Permissions
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

;;; One Window Toggle
(defvar modi/toggle-one-window--buffer-name nil
  "Variable to store the name of the buffer for which the `modi/toggle-one-window'
function is called.")
(defvar modi/toggle-one-window--window-configuration nil
  "Variable to store the window configuration before `modi/toggle-one-window'
function was called.")
(defun modi/toggle-one-window (&optional force-one-window)
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

;;; Kill/Bury Buffer

;; http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=2e4f4c9d48c563ff8bec102b66da0225587786c6
(>=e "26.0"
    nil  ;The `kill-current-buffer' command will be defined in core in emacs 26+
  (defun kill-current-buffer ()
    "Kill the current buffer.
When called in the minibuffer, get out of the minibuffer
using `abort-recursive-edit'.

This is like `kill-this-buffer', but it doesn't have to be invoked
via the menu bar, and pays no attention to the menu-bar's frame."
    (interactive)
    (let ((frame (selected-frame)))
      (if (and (frame-live-p frame)
               (not (window-minibuffer-p (frame-selected-window frame))))
          (kill-buffer (current-buffer))
        (abort-recursive-edit)))))

(defun modi/kill-buffer-dwim (kill-next-error-buffer)
  "Kill the current buffer.
When called in the minibuffer, get out of the minibuffer
using `abort-recursive-edit'.

If KILL-NEXT-ERROR-BUFFER is non-nil, kill the `next-error' buffer.
Examples of such buffers: *gtags-global*, *ag*, *Occur*."
  (interactive "P")
  (if kill-next-error-buffer
      (kill-buffer (next-error-find-buffer))
    (kill-current-buffer)))

(defun modi/quit-and-kill-window ()
  "Quit window and kill instead of burying the buffer in it."
  (interactive)
  (quit-window :kill))

;;; Other Window/Buffer
;; http://emacs.stackexchange.com/q/22226/115
(defhydra hydra-other-window-buffer
  (global-map "C-x"
              :color red)
  "other window/buffer"
  ("<right>" other-window "→win")
  ("<left>" (lambda () (interactive) (other-window -1)) "win←")
  ("<C-right>" next-buffer "→buf")
  ("<C-left>" previous-buffer "buf←"))

;;; *Messages* Auto-tail
;; Improved upon http://stackoverflow.com/a/4685005/1219634
(defun modi/messages-auto-tail (&rest _)
  "Make *Messages* buffer auto-scroll to the end after each message."
  (let* ((buf-name "*Messages*")
         ;; Create *Messages* buffer if it does not exist
         (buf (get-buffer-create buf-name)))
    ;; Activate this advice only if the point is _not_ in the *Messages* buffer
    ;; to begin with. This condition is required; otherwise you will not be
    ;; able to use `isearch' and other stuff within the *Messages* buffer as
    ;; the point will keep moving to the end of buffer :P
    (when (not (string= buf-name (buffer-name)))
      ;; Go to the end of buffer in all *Messages* buffer windows that are
      ;; *live* (`get-buffer-window-list' returns a list of only live windows).
      (dolist (win (get-buffer-window-list buf-name nil :all-frames))
        (with-selected-window win
          (goto-char (point-max))))
      ;; Go to the end of the *Messages* buffer even if it is not in one of
      ;; the live windows.
      (with-current-buffer buf
        (goto-char (point-max))))))
(advice-add 'message :after #'modi/messages-auto-tail)
;; (advice-remove 'message #'modi/messages-auto-tail)

;;; Bindings
;;;; Read-only Buffer Bindings
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

;;;; Other Bindings
(bind-keys
 :map modi-mode-map
  ("C-x 1"        . modi/toggle-one-window) ; default binding to `delete-other-windows'
  ;; overriding `C-x C-p' originally bound to `mark-page'
  ("C-x C-p"      . modi/copy-buffer-file-name)
  ;; overriding `C-x <delete>' originally bound to `backward-kill-sentence'
  ("C-x <delete>" . modi/delete-current-buffer-file)
  ("C-x C-r"      . rename-current-buffer-file)
  ("C-S-t"        . reopen-killed-file) ; mimick reopen-closed-tab in browsers
  ("C-c 6"        . reopen-killed-file) ; alternative to C-S-t for terminal mode
  ("C-("          . toggle-between-buffers)
  ("C-c ("        . toggle-between-buffers) ; alternative to C-( for terminal mode
  ("C-)"          . modi/kill-buffer-dwim)
  ("C-c )"        . modi/kill-buffer-dwim) ; alternative to C-) for terminal mode
  ("C-c 0"        . modi/kill-buffer-dwim)) ; alternative to C-) for terminal mode

;; Below bindings are made in global map as I want them to work even when my
;; minor mode is disabled
(bind-keys
 ("<f5>"   . revert-buffer)
 ("C-c 5"  . revert-buffer) ; alternative to f5
 ("<S-f5>" . modi/revert-all-file-buffers))

(bind-to-modi-map "b" #'modi/switch-to-scratch-and-back)
(bind-to-modi-map "f" #'modi/frame-setup-1)
(bind-to-modi-map "F" #'modi/frame-width-2x)
(bind-to-modi-map "y" #'bury-buffer)

(key-chord-define-global "XX" #'modi/kill-buffer-dwim)
(key-chord-define-global "ZZ" #'toggle-between-buffers)


(provide 'setup-windows-buffers)

;; TIPS

;; (1) `C-l'
;; C-l calls the `recenter-top-bottom' command. But typing C-l twice in a row
;; scrolls the window so that point is on the topmost screen line.  Typing a
;; third C-l scrolls the window so that point is on the bottom-most screen
;; line. Each successive C-l cycles through these three positions.
