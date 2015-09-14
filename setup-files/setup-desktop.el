;; Time-stamp: <2015-09-14 10:17:25 kmodi>

;; Desktop save and restore

(use-package desktop
  :config
  (progn
    (defvar modi/no-desktop-read-at-startup nil
      "Set this variable to a non-nil value if you do not want to enable
`desktop-save-mode'.

This variable can be used to start emacs without reading the previously
saved desktop at startup:

> emacs --eval \"(setq modi/no-desktop-read-at-startup t)\"
")

    (setq desktop-base-file-name (concat "emacs_" emacs-version-short
                                         ".desktop"))
    (setq desktop-base-lock-name (concat "emacs_" emacs-version-short
                                         ".desktop.lock"))

    ;; Fix the frameset warning at startup
    (setq desktop-restore-frames nil)

    ;; https://github.com/purcell/emacs.d/blob/master/lisp/init-sessions.el
    ;; Save a bunch of variables to the desktop file.
    ;; For lists, specify the length of the maximal saved data too.
    (setq desktop-globals-to-save
          (append '((comint-input-ring . 50)
                    desktop-missing-file-warning
                    (dired-regexp-history . 20)
                    (extended-command-history . 30)
                    (face-name-history . 20)
                    (file-name-history . 100)
                    (ido-buffer-history . 100)
                    (ido-last-directory-list . 100)
                    (ido-work-directory-list . 100)
                    (ido-work-file-list . 100)
                    (magit-read-rev-history . 50)
                    (minibuffer-history . 50)
                    (org-refile-history . 50)
                    (org-tags-history . 50)
                    (query-replace-history . 60)
                    (read-expression-history . 60)
                    (regexp-history . 60)
                    (regexp-search-ring . 20)
                    register-alist
                    (search-ring . 20)
                    (shell-command-history . 50)
                    ;; tags-file-name
                    ;; tags-table-list
                    )))

    ;; Don't save .gpg files. Restoring those files in emacsclients causes
    ;; a problem as the password prompt appears before the frame is loaded.
    (setq desktop-files-not-to-save
          (concat "\\(^/[^/:]*:\\|(ftp)$\\)" ; original value
                  "\\|\\(\\.gpg$\\)"
                  "\\|\\(\\.plstore$\\)"
                  "\\|\\(\\.desktop$\\)"
                  ;; FIXME
                  ;; If backup files with names like "file.sv.20150619_1641.bkp"
                  ;; are saved to the desktop file, emacsclient crashes at launch
                  ;; Need to debug why that's the case. But for now, simply not
                  ;; saving the .bkp files to the desktop file is a workable
                  ;; solution -- Fri Jun 19 16:45:50 EDT 2015 - kmodi
                  "\\|\\(\\.bkp$\\)"
                  "\\|\\(\\TAGS$\\)"))

    ;; Don't save the eww buffers
    (setq desktop-buffers-not-to-save (concat desktop-buffers-not-to-save
                                              "\\|\\(^eww\\(<[0-9]+>\\)*$\\)"))

    ;; Patch `desktop-restore-file-buffer'.
    ;; DON'T throw any warnings; especially "Note: file is write protected" when
    ;; restoring files from a saved desktop.
    (defun desktop-restore-file-buffer (buffer-filename
                                        _buffer-name
                                        _buffer-misc)
      "Restore a file buffer."
      (when buffer-filename
        (if (or (file-exists-p buffer-filename)
                (let ((msg (format "Desktop: File \"%s\" no longer exists."
                                   buffer-filename)))
                  (if desktop-missing-file-warning
                      (y-or-n-p (concat msg " Re-create buffer? "))
                    (message "%s" msg)
                    nil)))
            (let* ((auto-insert nil) ; Disable auto insertion
                   (coding-system-for-read
                    (or coding-system-for-read
                        (cdr (assq 'buffer-file-coding-system
                                   desktop-buffer-locals))))
                   (buf (find-file-noselect buffer-filename :nowarn))) ; <-- modified line
              (condition-case nil
                  (switch-to-buffer buf)
                (error (pop-to-buffer buf)))
              (and (not (eq major-mode desktop-buffer-major-mode))
                   (functionp desktop-buffer-major-mode)
                   (funcall desktop-buffer-major-mode))
              buf)
          nil)))

    (defun modi/restore-last-saved-desktop ()
      "Enable `desktop-save-mode' and restore the last saved desktop."
      (interactive)
      (desktop-save-mode 1)
      (desktop-read))

    (when (null modi/no-desktop-read-at-startup)
      (modi/restore-last-saved-desktop))

    (bind-keys
     :map modi-mode-map
      ("<S-f2>" . desktop-save-in-desktop-dir)
      ("<C-f2>" . modi/restore-last-saved-desktop))))


(provide 'setup-desktop)
