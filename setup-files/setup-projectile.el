;; Time-stamp: <2016-07-25 17:59:35 kmodi>

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

    ;; Customize the Projectile mode-line lighter
    ;; (setq projectile-mode-line "​P")
    ;; (setq projectile-mode-line '(:eval
    ;;                              (if (file-remote-p default-directory)
    ;;                                  "P"
    ;;                                (format "‹%s›" (projectile-project-name)))))

    ;; Show the projectile project name after the buffer name instead of in the
    ;; minor mode lighter area.
    (setq projectile-mode-line nil)
    (defconst modi/projectile-mode-line-project-name
      ;; Having this as `defvar' instead of `defconst' does not work for some
      ;; reason. -- Mon Jul 25 17:59:26 EDT 2016
      '("‹" (:eval (let ((prj (projectile-project-name)))
                     (if (file-remote-p default-directory)
                         "P"
                       (propertize prj 'face compilation-info-face))))
        "›")
      "Mode line construct for displaying current project name.")
    (defun modi/projectile-hook-fn ()
      "Function to run in `projectile-mode-hook'."
      (if projectile-mode
          (unless (memq 'modi/projectile-mode-line-project-name
                        mode-line-buffer-identification)
            (setq mode-line-buffer-identification
                  (append mode-line-buffer-identification
                          '(modi/projectile-mode-line-project-name))))
        (setq mode-line-buffer-identification
              (delq 'modi/projectile-mode-line-project-name
                    mode-line-buffer-identification))))
    (add-hook 'projectile-mode-hook #'modi/projectile-hook-fn)

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

    ;; Use `ag' all the time if available
    (defun modi/advice-projectile-use-ag ()
      "Always use `ag' for getting a list of all files in the project."
      (mapconcat 'identity
                 (append '("\\ag") ; used unaliased version of `ag': \ag
                         modi/ag-arguments
                         '("-0" ; output null separated results
                           "-g ''")) ; get file names matching the regex '' (all files)
                 " "))
    (when (executable-find "ag")
      (advice-add 'projectile-get-ext-command :override
                  #'modi/advice-projectile-use-ag))

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
             (prj-true (file-truename prj))
             (prj-abbr (abbreviate-file-name prj-true)))
        ;; First remove the current project from `projectile-known-projects'.
        ;; Also make sure that duplicate instance of the project name in form of symlink
        ;; name, true name and abbreviated name, if any, are also removed.
        (setq projectile-known-projects
              (delete prj (delete prj-true (delete prj-abbr projectile-known-projects))))
        ;; Then add back only the abbreviated true name to the beginning of
        ;; `projectile-known-projects'.
        (add-to-list 'projectile-known-projects prj-abbr)))
    (add-hook 'projectile-after-switch-project-hook #'modi/projectile-known-projects-sort)

    (defhydra hydra-projectile-other-window (:color teal)
      "projectile-other-window"
      ("f"  projectile-find-file-other-window        "file")
      ("g"  projectile-find-file-dwim-other-window   "file dwim")
      ("d"  projectile-find-dir-other-window         "dir")
      ("b"  projectile-switch-to-buffer-other-window "buffer")
      ("q"  nil                                      "cancel" :color blue))

    (defhydra hydra-projectile (:color teal
                                :hint  nil)
      "
     PROJECTILE: %(if (fboundp 'projectile-project-root) (projectile-project-root) \"TBD\")

^^^^       Find               ^^   Search/Tags       ^^^^       Buffers               ^^   Cache                     ^^^^       Other
^^^^--------------------------^^---------------------^^^^-----------------------------^^------------------------------------------------------------------
_f_/_s-f_: file               _a_: ag                ^^    _i_: Ibuffer               _c_: cache clear               ^^    _E_: edit project's .dir-locals.el
^^    _F_: file dwim          _g_: update gtags      ^^    _b_: switch to buffer      _x_: remove known project      _s-p_/_p_: switch to any other project
^^    _d_: file curr dir      _o_: multi-occur       _K_/_s-k_: kill all buffers      _X_: cleanup non-existing      ^^    _P_: switch to an open project
^^    _r_: recent file        ^^                     ^^^^                             _z_: cache current
^^    _D_: dir

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
      ("g"   ggtags-update-tags)
      ("i"   projectile-ibuffer)
      ("K"   projectile-kill-buffers)
      ("s-k" projectile-kill-buffers)
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

    (projectile-global-mode)))


(provide 'setup-projectile)

;; Do "touch ${PRJ_HOME}/.projectile" when updating a project. If the time stamp of
;; the ".projectile" is newer than that of the project cache then the existing
;; cache will be invalidated and recreated.
