;; Time-stamp: <2015-10-13 12:23:19 kmodi>

;; Deft is an Emacs mode for quickly browsing, filtering, and editing
;; directories of plain text notes, inspired by Notational Velocity.
;; http://jblevins.org/projects/deft
;; https://github.com/jrblevin/deft

(use-package deft
  :commands (deft deft-new-file deft-find-file)
  :bind (:map modi-mode-map
         ("C-c d" . modi/deft-dwim)
         ("<f6>"  . modi/deft-dwim))
  :config
  (progn
    (setq deft-directory (concat org-directory "notes/"))
    (setq deft-extensions '("org"))
    (setq deft-default-extension (copy-sequence (car deft-extensions)))

    (setq deft-use-filename-as-title nil) ; show actual titles in *Deft* buffer
    (setq deft-use-filter-string-for-filename t)

    (setq deft-auto-save-interval 0) ; default is 1.0, 0 to disable auto-save

    (setq deft-file-naming-rules '((nospace . "_")
                                   (case-fn . downcase)))

    (setq deft-strip-summary-regexp
          (concat "\\("
                  "[\n\t]" ; blank
                  "\\|^#\\+[[:upper:]_]+:.*$" ; org-mode metadata
                  "\\|^#\\s-+.*$" ; org-mode comments
                  ;; "\\|^\\s-*:[[:upper:]_]:.*$" ; org-mode properties
                  "\\)"))

    ;; http://pragmaticemacs.com/emacs/tweaking-deft-quicker-notes/
    (defvar modi/pre-deft-window-configuration nil
      "Variable to store the window configuration before `deft' was called.")

    ;; Advise deft to save window config
    (defun modi/deft-dwim-save-windows (orig-fun &rest args)
      (setq modi/pre-deft-window-configuration (current-window-configuration))
      (apply orig-fun args))
    (advice-add 'deft :around #'modi/deft-dwim-save-windows)

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

If OPEN-DEFT-BUFFER is non-nil, open `deft'.

Else if major-mode is `deft-mode', bury the buffer.
Else if in a deft-opened file buffer, call `modi/deft-quit'.
Else call `deft'."
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

    ;; Unbind the "C-o" key from `deft-mode-map'
    (define-key deft-mode-map (kbd "C-o") nil)
    (bind-keys
     :map deft-mode-map
      ("RET"        . modi/deft-complete)
      ("C-S-m"      . deft-new-file)
      ("<S-return>" . deft-new-file)
      ("C-c C-o"    . deft-open-file-other-window))))


(provide 'setup-deft)

;; Deft Mode bindings
;; |---------+------------------------------------------------------------------------|
;; | Binding | Description                                                            |
;; |---------+------------------------------------------------------------------------|
;; | C-c C-n | `deft-new-file' - Create new file quickly (based on filter if present) |
;; |         | without prompting the user.                                            |
;; | C-c C-m | `deft-new-file-named' - Create new file with name the user enters      |
;; | C-RET   | in the minibuffer.                                                     |
;; |---------+------------------------------------------------------------------------|
;; | C-c C-d | `deft-delete-file'                                                     |
;; | C-c C-r | `deft-rename-file'                                                     |
;; | C-c C-f | `deft-find-file'                                                       |
;; | C-c C-a | `deft-archive-file' - Move the file under point to                     |
;; |         | `deft-archive-directory'.                                              |
;; |---------+------------------------------------------------------------------------|
;; | C-c C-l | `deft-filter' - Enter the filter string in the minibuffer.             |
;; | C-c C-c | `deft-filter-clear' - Reset the filter.                                |
;; |---------+------------------------------------------------------------------------|
;; | C-c C-t | `deft-toggle-incremental-search'                                       |
;; | C-c C-s | `deft-toggle-sort-method'                                              |
;; |---------+------------------------------------------------------------------------|
;; | C-c C-g | `deft-refresh'                                                         |
;; | C-c C-q | `quit-window'                                                          |
;; |---------+------------------------------------------------------------------------|
