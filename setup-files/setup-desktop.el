;; Time-stamp: <2016-08-09 11:45:44 kmodi>

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

    ;; Wed Jul 20 09:36:15 EDT 2016 - kmodi
    ;; Below problem seems to be fixed in emacs 25.x
    (>=e "25.0"
        nil
      ;; Fix the frameset error at startup
      (setq desktop-restore-frames nil))

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
    (let (;; http://thread.gmane.org/gmane.emacs.devel/202463/focus=202496
          (default (eval (car (get 'desktop-buffers-not-to-save 'standard-value))))
          (eww-buf-regexp "\\(^eww\\(<[0-9]+>\\)*$\\)"))
      (setq desktop-buffers-not-to-save (concat default
                                                "\\|" eww-buf-regexp)))

    ;; http://emacs.stackexchange.com/a/20036/115
    (defun modi/bury-star-buffers ()
      "Bury all star buffers."
      (mapc (lambda (buf)
              (when (string-match-p "\\`\\*.*\\*\\'" (buffer-name buf))
                (bury-buffer buf)))
            (buffer-list)))
    (add-hook 'desktop-after-read-hook #'modi/bury-star-buffers)

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
