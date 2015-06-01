;; Time-stamp: <2015-06-01 13:48:54 kmodi>

;; Deft is an Emacs mode for quickly browsing, filtering, and editing
;; directories of plain text notes, inspired by Notational Velocity.
;; http://jblevins.org/projects/deft/

(use-package deft
  :config
  (progn
    (setq deft-directory (concat org-directory "notes/"))
    (setq deft-extension "org")
    (setq deft-text-mode 'org-mode)
    (setq deft-use-filename-as-title nil)
    (setq deft-auto-save-interval 1.0) ; default: 1.0; 0 to disable auto-save

    (defun deft-absolute-filename (slug &optional extension)
      "Return an absolute filename to file named SLUG with optional EXTENSION.
If EXTENSION is not given, `deft-extension' is assumed.

If SLUG has spaces, replace them with underscores.
Also force the file name to be all lower case."
      (let* ((slug-no-space (replace-regexp-in-string
                             "\\(.*?\\) *\\'" "\\1"
                             slug)) ; remove trailing spaces if any
             (slug-no-space (replace-regexp-in-string
                             " " "_"
                             slug-no-space)) ; replace spaces with _
             (slug-no-space (downcase slug-no-space))) ; lower case
        (concat (file-name-as-directory (expand-file-name deft-directory))
                slug-no-space
                "." (or extension deft-extension))))

    ;; http://pragmaticemacs.com/emacs/tweaking-deft-quicker-notes/
    (defvar modi/pre-deft-window-configuration nil
      "Variable to store the window configuration before `deft' was called.")

    ;; Advise deft to save window config
    (defun modi/deft-dwim-save-windows (orig-fun &rest args)
      (setq modi/pre-deft-window-configuration (current-window-configuration))
      (apply orig-fun args))
    (advice-add 'deft :around #'modi/deft-dwim-save-windows)

    (defun modi/quit-deft ()
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

If major-mode is `deft-mode', call `deft-delete-file'.
If in a deft-opened file buffer, call `modi/quit-deft'.
Else call `deft'.

With prefix argument, open `deft'."
      (interactive "P")
      (cond
       (open-deft-buffer ; when using `C-u'
        (deft))
       ((derived-mode-p 'deft-mode)
        (deft-delete-file))
       ;; If the user is in a file buffer opened by deft,
       ;; - `modi/pre-deft-window-configuration' will be non-nil, AND
       ;; - the buffer name would have been added to `deft-auto-save-buffers'
       ;;   by the `deft-open-file' function (whether the user has chosen to
       ;;   auto save the deft files or not).
       ((and modi/pre-deft-window-configuration
             (member (get-buffer (buffer-name)) deft-auto-save-buffers))
        (modi/quit-deft))
       (t
        (deft))))
    (bind-key "C-c C-d" #'modi/deft-dwim modi-mode-map)))


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
