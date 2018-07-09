;; Time-stamp: <2018-07-09 17:54:19 kmodi>

;; Projectile
;; https://github.com/bbatsov/projectile

(use-package projectile
  :bind (:map modi-mode-map
         ("C-c p" . hydra-projectile/body)
         ("C-c f" . hydra-projectile/body)
         ("s-f"   . hydra-projectile/body))
  :commands (projectile-project-root)
  :init
  (progn
    (setq projectile-keymap-prefix (kbd "C-c P")))
  :config
  (progn
    (when (not (bound-and-true-p disable-pkg-ivy))
      (with-eval-after-load 'ivy
        (setq projectile-completion-system 'ivy)))

    ;; Do not barf when I try to do `projectile-switch-project' while in a
    ;; buffer containing a non-projectile file.
    (setq projectile-require-project-root nil)

    ;; Don't consider my home dir as a project
    (add-to-list 'projectile-ignored-projects `,(concat (getenv "HOME") "/"))

    ;; (setq projectile-enable-caching nil)
    (setq projectile-enable-caching t) ; Enable caching, otherwise
                                        ; `projectile-find-file' is really slow
                                        ; for large projects.
    (dolist (item '(".SOS" "nobackup"))
      (add-to-list 'projectile-globally-ignored-directories item))
    (dolist (item '("GTAGS" "GRTAGS" "GPATH"))
      (add-to-list 'projectile-globally-ignored-files item))

    ;; Git projects should be marked as projects in top-down fashion,
    ;; so that each git submodule can be a projectile project.
    (setq projectile-project-root-files-bottom-up
          (delete ".git" projectile-project-root-files-bottom-up))
    (add-to-list 'projectile-project-root-files ".git")

    (setq projectile-project-root-files-functions
          '(projectile-root-local
            projectile-root-top-down ; First look for projects in top-down order
            projectile-root-bottom-up)) ; Then in bottom-up order

    ;; Customize the Projectile mode-line lighter
    (setq projectile-mode-line "​P")
    ;; (setq projectile-mode-line '(:eval
    ;;                              (if (file-remote-p default-directory)
    ;;                                  "P"
    ;;                                (format "‹%s›" (projectile-project-name)))))

    (defun modi/projectile-project-name (project-root)
      "Return project name after some modification if needed.

If PROJECT-ROOT contains \"sos_\" or \"src\", remove the directory basename
from the project root name. E.g. if PROJECT-ROOT is \"/a/b/src\", remove the
\"src\" portion from it and make it \"/a/b\"."
      (let ((project-name (projectile-default-project-name project-root)))
        (while (string-match "\\(sos_\\|\\bsrc\\b\\)" project-name)
          (setq project-root (replace-regexp-in-string
                              "\\(.*\\)/.+/*$" "\\1" project-root))
          (setq project-name (file-name-nondirectory
                              (directory-file-name project-root))))
        project-name))
    (setq projectile-project-name-function #'modi/projectile-project-name)

    (defun modi/advice-projectile-use-ag ()
      "Always use `ag' for getting a list of all files in the project."
      (mapconcat #'shell-quote-argument
                 (append '("ag")
                         modi/ag-arguments
                         '("-0"         ;Output null separated results
                           "-g" ""))    ;Get file names matching "" (all files)
                 " "))

    (defun modi/advice-projectile-use-rg ()
      "Always use `rg' for getting a list of all files in the project."
      (let* ((prj-user-ignore-name (expand-file-name
                                    (concat ".ignore." user-login-name)
                                    (projectile-project-root)))
             (prj-user-ignore (when (file-exists-p prj-user-ignore-name)
                                (concat "--ignore-file " prj-user-ignore-name))))
        (mapconcat #'shell-quote-argument
                   (if prj-user-ignore
                       (append '("rg")
                               modi/rg-arguments
                               `(,prj-user-ignore)
                               '("--null" ;Output null separated results
                                 ;; Get names of all the to-be-searched files,
                                 ;; same as the "-g ''" argument in ag.
                                 "--files"))
                     (append '("rg")
                             modi/rg-arguments
                             '("--null"
                               "--files")))
                   " ")))

    ;; Use `rg' all the time if available
    (if (executable-find "rg")
        (progn
          (advice-remove 'projectile-get-ext-command #'modi/advice-projectile-use-ag)
          (advice-add 'projectile-get-ext-command :override #'modi/advice-projectile-use-rg))
      ;; Else use `ag' if available
      (when (executable-find "ag")
        (advice-remove 'projectile-get-ext-command #'modi/advice-projectile-use-rg)
        (advice-add 'projectile-get-ext-command :override #'modi/advice-projectile-use-ag)))

    ;; Make the file list creation faster by NOT calling `projectile-get-sub-projects-files'
    (defun modi/advice-projectile-no-sub-project-files ()
      "Directly call `projectile-get-ext-command'. No need to try to get a
list of sub-project files if the vcs is git."
      (projectile-files-via-ext-command (projectile-get-ext-command)))
    (advice-add 'projectile-get-repo-files :override
                #'modi/advice-projectile-no-sub-project-files)

    ;; Do not visit the current project's tags table if `ggtags-mode' is loaded.
    ;; Doing so prevents the unnecessary call to `visit-tags-table' function
    ;; and the subsequent `find-file' call for the `TAGS' file."
    (defun modi/advice-projectile-dont-visit-tags-table ()
      "Don't visit the tags table as we are using gtags/global."
      nil)
    (when (fboundp 'ggtags-mode)
      (advice-add 'projectile-visit-project-tags-table :override
                  #'modi/advice-projectile-dont-visit-tags-table))

    ;; http://emacs.stackexchange.com/a/10187/115
    (defun modi/kill-non-project-buffers (&optional kill-special)
      "Kill buffers that do not belong to a `projectile' project.

With prefix argument (`C-u'), also kill the special buffers."
      (interactive "P")
      (let ((bufs (buffer-list (selected-frame))))
        (dolist (buf bufs)
          (with-current-buffer buf
            (let ((buf-name (buffer-name buf)))
              (when (or (null (projectile-project-p))
                        (and kill-special
                             (string-match "^\*" buf-name)))
                ;; Preserve buffers with names starting with *scratch or *Messages
                (unless (string-match "^\\*\\(\\scratch\\|Messages\\)" buf-name)
                  (message "Killing buffer %s" buf-name)
                  (kill-buffer buf))))))))

    (defun modi/package-make-projectile-cache-stale (install-list delete-list)
      "Mark the .emacs.d projectile project to be updated by updating
the .projectile file for that project, when any package is installed or deleted.
The return value of this function is unused as it is added as an :after advice."
      ;; Update the .projectile file if any package is installed or deleted.
      (when (or install-list delete-list)
        (write-region "" :ignore (concat user-emacs-directory ".projectile")
                      nil :no-message-echo)))
    (advice-add 'package-menu--perform-transaction
                :after #'modi/package-make-projectile-cache-stale)
    (with-eval-after-load 'paradox
      (advice-add 'paradox--perform-package-transaction
                  :after #'modi/package-make-projectile-cache-stale))

    (defun modi/projectile-known-projects-sort ()
      "Move the now current project to the top of the `projectile-known-projects' list."
      (let* ((prj (projectile-project-root))
             ;; Set `prj' to nil if that project is supposed to be ignored
             (prj (and (not (projectile-ignored-project-p prj)) prj))
             (prj-true (and prj (file-truename prj)))
             (prj-abbr (and prj (abbreviate-file-name prj-true))))
        (when prj
          ;; First remove the current project from `projectile-known-projects'.
          ;; Also make sure that duplicate instance of the project name in form of symlink
          ;; name, true name and abbreviated name, if any, are also removed.
          (setq projectile-known-projects
                (delete prj (delete prj-true (delete prj-abbr projectile-known-projects))))
          ;; Then add back only the abbreviated true name to the beginning of
          ;; `projectile-known-projects'.
          (add-to-list 'projectile-known-projects prj-abbr))))
    (add-hook 'projectile-after-switch-project-hook #'modi/projectile-known-projects-sort)

    (defun modi/projectile-find-file-literally (&optional arg)
      "Jump to a project's file literally (see `find-file-literally') using
completion.  With a prefix ARG invalidates the cache first.

Using this function over `projectile-find-file' is useful for opening files that
are slow to open because of their major mode. `find-file-literally' always opens
files in Fundamental mode."
      (interactive "P")
      (projectile-maybe-invalidate-cache arg)
      (projectile-completing-read
       "Find file literally: "
       (projectile-current-project-files)
       :action `(lambda (file)
                  (find-file-literally (expand-file-name file ,(projectile-project-root)))
                  (run-hooks 'projectile-find-file-hook))))

    (defun modi/projectile-switch-project-magit-status ()
      "Switch to other project and open Magit status there."
      (interactive)
      (let ((projectile-switch-project-action #'magit-status))
        (call-interactively #'projectile-switch-project)))

    (defhydra hydra-projectile-other-window (:color teal)
      "projectile-other-window"
      ("b" projectile-switch-to-buffer-other-window "buffer")
      ("D" projectile-find-dir-other-window "dir")
      ("f" projectile-find-file-other-window "file")
      ("F" projectile-find-file-dwim-other-window "file dwim")
      ("q" nil "cancel" :color blue))

    (defhydra hydra-projectile (:color teal
                                :hint  nil)
      "
     PROJECTILE: %(if (fboundp 'projectile-project-root) (projectile-project-root) \"TBD\")

^^^^       Find               ^^   Search/Tags       ^^^^       Buffers               ^^   Cache                     ^^^^       Other
^^^^--------------------------^^---------------------^^^^-----------------------------^^------------------------------------------------------------------
_f_/_s-f_: file               _a_: ag                ^^    _i_: Ibuffer               _c_: cache clear               ^^    _E_: edit project's .dir-locals.el
^^    _F_: file dwim          _G_: update gtags      ^^    _b_: switch to buffer      _x_: remove known project      _s-p_/_p_: switch to other project
^^    _d_: file curr dir      _o_: multi-occur       _K_/_s-k_: kill all buffers      _X_: cleanup non-existing      ^^    _g_: switch to Magit status of other project
^^    _l_: file literally     ^^                     ^^^^                             _z_: cache current             ^^    _P_: switch to an open project
^^    _r_: recent file        ^^                     ^^^^                             ^^                             ^^    _D_: find dir
"
      ("a"   projectile-ag)
      ("b"   projectile-switch-to-buffer)
      ("c"   projectile-invalidate-cache)
      ("d"   projectile-find-file-in-directory)
      ("f"   projectile-find-file)
      ("s-f" projectile-find-file)
      ("F"   projectile-find-file-dwim)
      ("D"   projectile-find-dir)
      ("E"   projectile-edit-dir-locals)
      ("g"   modi/projectile-switch-project-magit-status)
      ("G"   ggtags-update-tags)
      ("i"   projectile-ibuffer)
      ("K"   projectile-kill-buffers)
      ("s-k" projectile-kill-buffers)
      ("l"   modi/projectile-find-file-literally)
      ("m"   projectile-multi-occur)
      ("o"   projectile-multi-occur)
      ("p"   projectile-switch-project)
      ("s-p" projectile-switch-project)
      ("P"   projectile-switch-open-project)
      ("s"   projectile-switch-project)
      ("r"   projectile-recentf)
      ("x"   projectile-remove-known-project)
      ("X"   projectile-cleanup-known-projects)
      ("z"   projectile-cache-current-file)
      ("4"   hydra-projectile-other-window/body "other window")
      ("q"   nil "cancel" :color blue))

    (projectile-mode)))


(provide 'setup-projectile)

;; Do "touch ${PRJ_HOME}/.projectile" when updating a project. If the time stamp of
;; the ".projectile" is newer than that of the project cache then the existing
;; cache will be invalidated and recreated.
