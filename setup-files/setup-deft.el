;; Time-stamp: <2015-06-18 00:41:49 kmodi>

;; Deft is an Emacs mode for quickly browsing, filtering, and editing
;; directories of plain text notes, inspired by Notational Velocity.
;; http://jblevins.org/projects/deft/

(use-package deft
  ;; :load-path "elisp/deft"
  :commands (modi/deft-dwim deft deft-new-file deft-find-file)
  :init
  (progn
    (bind-key "C-c d" #'modi/deft-dwim modi-mode-map)
    (bind-key "<f6>"  #'modi/deft-dwim modi-mode-map))
  :config
  (progn
    (setq deft-directory (concat org-directory "notes/"))
    (setq deft-extension "org")
    (setq deft-text-mode 'org-mode)
    (setq deft-use-filename-as-title nil) ; show actual titles in deft buf
    (setq deft-use-filter-string-for-filename t)
    (setq deft-auto-save-interval 0) ; default: 1.0; 0 to disable auto-save
    (setq deft-file-naming-rules '((nospace . "_")
                                   (case-fn . downcase)))

    ;; http://pragmaticemacs.com/emacs/tweaking-deft-quicker-notes/
    (defvar modi/pre-deft-window-configuration nil
      "Variable to store the window configuration before `deft' was called.")

    ;; Advise deft to save window config
    (defun modi/deft-dwim-save-windows (orig-fun &rest args)
      (setq modi/pre-deft-window-configuration (current-window-configuration))
      (apply orig-fun args))
    (advice-add #'deft :around #'modi/deft-dwim-save-windows)

    (defun modi/deft-quit ()
      "Save buffer, kill both the deft-opened file buffer and the *Deft* buffer,
and restore the window config to the way it was before deft was invoked."
      (interactive)
      (let ((buf (buffer-name)))
        (save-buffer)
        (kill-buffer buf)
        (delq buf deft-auto-save-buffers) ; Remove the buffer from `deft-auto-save-buffers'
        (kill-buffer "*Deft*")
        (when (window-configuration-p modi/pre-deft-window-configuration)
          (set-window-configuration modi/pre-deft-window-configuration)
          ;; Reset `modi/pre-deft-window-configuration' back to `nil' because
          ;; that value is one of the criteria to check if the user is currently
          ;; editing a deft-opened file
          (setq modi/pre-deft-window-configuration nil))))

    (defun modi/deft-dwim (open-deft-buffer)
      "Launch deft or quit a deft opened file based on context.

If major-mode is `deft-mode', bury the buffer.
If in a deft-opened file buffer, call `modi/deft-quit'.
Else call `deft'.

With prefix argument, open `deft'."
      (interactive "P")
      (cond
       (open-deft-buffer ; when using `C-u'
        (deft))
       ((derived-mode-p 'deft-mode)
        (bury-buffer))
       ;; If the user is in a file buffer opened by deft,
       ;; - `modi/pre-deft-window-configuration' will be non-nil, AND
       ;; - the buffer name would have been added to `deft-auto-save-buffers'
       ;;   by the `deft-open-file' function (whether the user has chosen to
       ;;   auto save the deft files or not).
       ((and modi/pre-deft-window-configuration
             (member (get-buffer (buffer-name)) deft-auto-save-buffers))
        (modi/deft-quit))
       (t
        (deft))))

    (defun modi/deft-complete (new-file)
      "Call the `deft-complete' command by default.
If NEW-FILE is non-nil, call `deft-new-file'."
      (interactive "P")
      (if new-file
          (deft-new-file)
        (deft-complete)))
    (bind-key "RET" #'modi/deft-complete deft-mode-map)))


(provide 'setup-deft)

;; |-------------------+--------------------------------|
;; | deft mode binding | Description                    |
;; |-------------------+--------------------------------|
;; | C-c C-n           | deft-new-file                  |
;; | C-c C-m           | deft-new-file-named            |
;; | <C-return>        | deft-new-file-named            |
;; | C-c C-d           | deft-delete-file               |
;; | C-c C-r           | deft-rename-file               |
;; | C-c C-f           | deft-find-file                 |
;; | C-c C-a           | deft-archive-file              |
;; | C-c C-t           | deft-toggle-incremental-search |
;; | C-c C-s           | deft-toggle-sort-method        |
;; | C-c C-g           | deft-refresh                   |
;; | C-c C-q           | quit-window                    |
;; |-------------------+--------------------------------|
