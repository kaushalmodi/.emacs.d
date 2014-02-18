;; Time-stamp: <2014-02-18 12:32:05 kmodi>

;; Desktop save and restore

;; Type ‘M-x session-save’, or ‘M-x session-restore’ whenever you want to save
;; or restore a desktop. Restored desktops are deleted from disk.

(desktop-save-mode 1)
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

;; Source: https://github.com/purcell/emacs.d/blob/master/lisp/init-sessions.el
; save a bunch of variables to the desktop file
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


(setq setup-desktop-loaded t)
(provide 'setup-desktop)
