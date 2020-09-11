;; Time-stamp: <2020-09-10 23:11:53 kmodi>

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

    ;; Immediately load 10 buffers and lazy-load the rest.
    (setq desktop-restore-eager 10)

    (setq desktop-base-file-name (concat "emacs_" emacs-version-short
                                         ".desktop"))
    (setq desktop-base-lock-name (concat "emacs_" emacs-version-short
                                         ".desktop.lock"))

    ;; Wed Jul 20 09:36:15 EDT 2016 - kmodi
    ;; Below problem seems to be fixed in emacs 25.x
    ;; Sat Feb 18 16:31:50 EST 2017 - kmodi
    ;; Nope! It's not! https://github.com/purcell/emacs.d/issues/259
    ;; The problem occurs when opening emacs in -nw mode, saving desktop,
    ;; quitting, and then reopening emacs in GUI mode. I get this in a
    ;; *Warning* buffer:
    ;;     Error (frameset): Font ‘tty’ is not defined
    (>=e "25.0"
        nil
      ;; 'Fix' the frameset error at startup
      ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=17352
      (setq desktop-restore-frames nil))

    ;; Wed Apr 12 11:30:03 EDT 2017 - kmodi
    ;; Something changed in emacs master in the last week or so. If
    ;; `desktop-restore-frames' is non-nil, nlinum get enabled in the
    ;; beginning, but then it gets disabled.
    ;; (setq desktop-restore-frames nil)
    ;; Wed Apr 12 11:34:37 EDT 2017 - kmodi
    ;; And then.. commenting out the above again.. magically the nlinum
    ;; issue went away.. may be corrupt desktop files?

    ;; Fix the below error when starting emacs:
    ;;   Error (frameset): Wrong type argument: number-or-marker-p, nil
    ;; This error appears when switching emacs use between -nw and GUI modes.
    ;; http://stackoverflow.com/a/26546872/1219634
    (setq desktop-restore-forces-onscreen nil)

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

    (let ((default (eval (car (get 'desktop-files-not-to-save 'standard-value)))))
      (setq desktop-files-not-to-save
            (eval
             `(rx (or (regexp ,default)
                      (and (or ".plstore"
                               ".desktop"
                               ;; Don't save .gpg files. Restoring those files
                               ;; in emacsclient causes a problem as the
                               ;; password prompt appears before the frame is
                               ;; loaded.
                               ".gpg"
                               ;; FIXME
                               ;; If backup files with names like
                               ;; "file.sv.1.bkp" are saved to the desktop file,
                               ;; emacsclient crashes at launch Need to debug
                               ;; why that's the case. But for now, simply not
                               ;; saving the .bkp files to the desktop file
                               ;; works -- Fri Jun 19 16:45:50 EDT 2015
                               ".bkp"
                               ;; I do not typically plan to re-open the .el.gz
                               ;; files opened in my previous sessions.
                               ".el.gz"
                               ;; Don't auto-open .ckt files as the Spice mode
                               ;; loading is time-consuming
                               ".ckt"
                               "TAGS")
                           line-end))))))

    ;; I use `modi/desktop-files-not-to-save' to set additional regexp for my
    ;; work files.
    (when (bound-and-true-p modi/desktop-files-not-to-save)
      (setq desktop-files-not-to-save
            (eval
             `(rx (or (regexp ,desktop-files-not-to-save)
                      (regexp ,modi/desktop-files-not-to-save))))))

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
      ;; Thu Sep 10 22:52:47 EDT 2020 - kmodi
      ;; Prevent Desktop conflict messages "Desktop file is more recent .."
      ;; Desktop file is more recent ((24410 58051 572466 0),(51 36 22 10 9 2020 4 t -14400))
      ;; than the one loaded ((24410 58051 572462 0),(51 36 22 10 9 2020 4 t -14400)).
      ;; Save anyway? y
      (desktop-auto-save-disable)
      (desktop-read))

    (when (null modi/no-desktop-read-at-startup)
      (modi/restore-last-saved-desktop))

    (bind-keys
     :map modi-mode-map
     ("<S-f2>" . desktop-save-in-desktop-dir)
     ("<C-f2>" . modi/restore-last-saved-desktop))))


(provide 'setup-desktop)
