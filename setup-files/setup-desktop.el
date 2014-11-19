;; Time-stamp: <2014-11-19 12:50:38 kmodi>

;; Desktop save and restore

;; Type ‘M-x session-save’, or ‘M-x session-restore’ whenever you want to save
;; or restore a desktop. Restored desktops are deleted from disk.

(req-package desktop
  :config
  (progn
    (desktop-save-mode 1)

    ;; Source: https://github.com/purcell/emacs.d/blob/master/lisp/init-sessions.el
    ;; save a bunch of variables to the desktop file
    ;; for lists specify the len of the maximal saved data also
    (setq desktop-globals-to-save
          (append '((comint-input-ring . 50)
                    (compile-history . 30)
                    desktop-missing-file-warning
                    (dired-regexp-history . 20)
                    (extended-command-history . 30)
                    (face-name-history . 20)
                    (file-name-history . 100)
                    (grep-find-history . 30)
                    (grep-history . 30)
                    (ido-buffer-history . 100)
                    (ido-last-directory-list . 100)
                    (ido-work-directory-list . 100)
                    (ido-work-file-list . 100)
                    (magit-read-rev-history . 50)
                    (minibuffer-history . 50)
                    (org-clock-history . 50)
                    (org-refile-history . 50)
                    (org-tags-history . 50)
                    (query-replace-history . 60)
                    (read-expression-history . 60)
                    (regexp-history . 60)
                    (regexp-search-ring . 20)
                    register-alist
                    (search-ring . 20)
                    (shell-command-history . 50)
                    tags-file-name
                    tags-table-list)))

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

    (defun save-desktop-save-buffers-kill-emacs ()
      "Save buffers and current desktop every time when quitting emacs."
      (interactive)
      (desktop-save-in-desktop-dir)
      (save-buffers-kill-emacs))

    (bind-keys
     :map modi-mode-map
     ("<S-f2>" . desktop-save-in-desktop-dir))))


(provide 'setup-desktop)

;; (desktop-save-mode nil)
;; comment (desktop-save-mode 1) is you want to use only one desktop
;; using the below code

;; ;; use only one desktop
;; (setq desktop-dirname user-emacs-directory)
;; (setq desktop-path (list desktop-dirname))
;; (setq desktop-base-file-name "emacs-desktop")
;; (setq desktop-file-name (concat desktop-dirname "/" desktop-base-file-name))

;; ;; remove desktop after it's been read
;; (add-hook 'desktop-after-read-hook
;; 	  '(lambda ()
;; 	     ;; desktop-remove clears desktop-dirname
;; 	     (setq desktop-dirname-tmp desktop-dirname)
;; 	     (desktop-remove)
;; 	     (setq desktop-dirname desktop-dirname-tmp)))

;; (defun saved-session ()
;;   (file-exists-p desktop-file-name))

;; ;; use session-restore to restore the desktop manually
;; (defun session-restore ()
;;   "Restore a saved emacs session."
;;   (interactive)
;;   (if (saved-session)
;;       (desktop-read)
;;     (message "No desktop found.")))

;; ;; use session-save to save the desktop manually
;; (defun session-save (&optional noconfirm)
;;   "Save an emacs session."
;;   (interactive)
;;   (if (saved-session)
;;       (if noconfirm
;;           (desktop-save-in-desktop-dir)
;;         ;; else
;;         (if (y-or-n-p "Overwrite existing desktop? ")
;;             (desktop-save-in-desktop-dir)
;;           (message "Session not saved.")))
;;     (desktop-save-in-desktop-dir)))

;; ;; ask user whether to restore desktop at start-up
;; (add-hook 'after-init-hook
;; 	  '(lambda ()
;; 	     (if (saved-session)
;; 		 (if (y-or-n-p "Restore desktop? ")
;; 		     (session-restore)))))
