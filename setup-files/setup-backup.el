;; Time-stamp: <2022-06-17 14:19:51 kmodi>

;; Backups

;; When `vc-make-backup-files' is nil (default), backups are not made for
;; version controlled (e.g. git) files. Set this to `t' to allow making backups
;; using prefix args to `save-buffer' command.
(setq vc-make-backup-files t)

;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Numbered-Backups.html
;; http://stackoverflow.com/a/151946/1219634
;; `version-control' is nil by default; numbered backups are made only if the
;; the visited file already has numbered backups. Below will always create
;; numbered backups.
(setq version-control :make-numbered-backups)

(setq kept-new-versions 20) ; default 2
(setq kept-old-versions 5) ; default 2
;; If there are backups numbered 1, 2, 3, 5, and 7, and both of the above
;; variables have the value 2, then the backups numbered 1 and 2 are kept
;; as old versions and those numbered 5 and 7 are kept as new versions;
;; backup version 3 is deleted after user confirmation.

;; Excess backup deletion will happen silently, without user confirmation, if
;; `delete-old-versions' is set to `t'.
(setq delete-old-versions t) ; default nil

;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Rename-or-Copy.html
(setq backup-by-copying t) ; don't clobber symlinks

(defvar modi/backup-directory (let ((dir (file-name-as-directory (expand-file-name ".backups" modi/temporary-file-directory))))
                                (make-directory dir :parents)
                                dir)
  "Directory for storing my backups.")

;; Save all backups to `modi/backup-directory'
(setq backup-directory-alist `(("." . ,modi/backup-directory)))
(message (format "All backup files will be saved to %s." modi/backup-directory))

;; http://ergoemacs.org/emacs/elisp_make-backup.html
(defun modi/make-backup ()
  "Make a backup copy of current file.
The backup file name has the form ‹name›~‹timestamp›~, in the same dir.
If such a file already exist, it's overwritten.
If the current buffer is not associated with a file, nothing's done."
  (interactive)
  (if (buffer-file-name)
      (let* ((currentName (buffer-file-name))
             (backupName (concat currentName
                                 "." (format-time-string "%Y%m%d_%H%M") ".bkp")))
        (copy-file currentName backupName :overwrite-if-already-exists)
        (message (concat "Backup saved as: " (file-name-nondirectory backupName))))
    (user-error "buffer is not a file.")))
;; Also make emacs ignore that appended string to the backup files when
;; deciding the major mode
;; http://emacs.stackexchange.com/a/13285/115
(add-to-list 'auto-mode-alist '("\\.[0-9_]+\\.bkp\\'" nil backup-file))
(bind-to-modi-map "`" #'modi/make-backup)


(provide 'setup-backup)
