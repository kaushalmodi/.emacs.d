;; Time-stamp: <2015-07-16 09:41:07 kmodi>

;; Projectile
;; Source: https://github.com/bbatsov/projectile

(use-package projectile
  :init
  (progn
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

^^^^       Find               ^^   Search/Tags       ^^^^       Buffers               ^^   Cache
^^^^--------------------------^^---------------------^^^^-----------------------------^^-----------------------
_f_/_s-f_: file               _a_: ag                ^^    _i_: Ibuffer               _c_: cache clear
^^    _F_: file dwim          _g_: update gtags      ^^    _b_: switch to buffer      _x_: remove known project
^^    _d_: file curr dir      _o_: multi-occur       _K_/_s-k_: Kill all buffers      _X_: cleanup non-existing
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
      ("g"   ggtags-update-tags)
      ("i"   projectile-ibuffer)
      ("K"   projectile-kill-buffers)
      ("s-k" projectile-kill-buffers)
      ("m"   projectile-multi-occur)
      ("o"   projectile-multi-occur)
      ("p"   projectile-switch-project "switch project")
      ("s-p" projectile-switch-project "switch project")
      ("s"   projectile-switch-project)
      ("r"   projectile-recentf)
      ("x"   projectile-remove-known-project)
      ("X"   projectile-cleanup-known-projects)
      ("z"   projectile-cache-current-file)
      ("`"   hydra-projectile-other-window/body "other window")
      ("q"   nil "cancel" :color blue))
    (bind-key "s-f" #'hydra-projectile/body modi-mode-map)
    (setq projectile-keymap-prefix (kbd "C-c P"))
    (bind-key "C-c p" #'hydra-projectile/body modi-mode-map)
    (bind-key "C-c f" #'hydra-projectile/body modi-mode-map))
  :config
  (progn
    (when (not (bound-and-true-p disable-pkg-ivy))
      (with-eval-after-load 'ivy
        (setq projectile-completion-system 'ivy)))

    (defvar projectile-ag-command
      (concat "\\ag" ; used unaliased version of `ag': \ag
              " -i" ; case insensitive
              " -f" ; follow symbolic links
              " --skip-vcs-ignores" ; Ignore files/dirs ONLY from `.agignore',
              " -0" ; output null separated results
              " -g ''") ; get file names matching the regex ''
      "Ag command to be used by projectile to generate file cache.")

    (when (executable-find "ag")
      (defun projectile-get-ext-command ()
        "Override the projectile-defined function so that `ag' is always used for
getting a list of all files in a project."
        projectile-ag-command))

    ;; Make the file list creation faster by NOT calling `projectile-get-sub-projects-files'
    (defun projectile-get-repo-files ()
      (projectile-files-via-ext-command (projectile-get-ext-command)))

    ;; Don't consider my home dir as a project
    (add-to-list 'projectile-ignored-projects `,(concat (getenv "HOME") "/"))

    (defun projectile-cache-files-find-file-hook ()
      "Function for caching files with `find-file-hook'."
      (when (and projectile-enable-caching
                 (projectile-project-p)
                 (not (member (projectile-project-p) projectile-ignored-projects)))
        (projectile-cache-current-file)))

    ;; (setq projectile-enable-caching nil)
    (setq projectile-enable-caching t) ; Enable caching, otherwise
                                        ; `projectile-find-file' is really slow
                                        ; for large projects
    (dolist (item '(".SOS" "nobackup"))
      (add-to-list 'projectile-globally-ignored-directories item))
    (dolist (item '("GTAGS" "GRTAGS" "GPATH"))
      (add-to-list 'projectile-globally-ignored-files item))
    ;; Customize the Projectile mode-line lighter
    ;; (setq projectile-mode-line '(:eval (format " Projectile[%s]" (projectile-project-name))))
    ;; (setq projectile-mode-line " ∏")
    ;; (setq projectile-mode-line " ℙ")
    (setq projectile-mode-line "​P")

    (when (fboundp 'ggtags-mode)
      (defun projectile-visit-project-tags-table ()
        "Visit the current project's tags table ONLY IF `ggtags-mode' is not loaded.
If `ggtags-mode' function is defined, “empty” this definition so that it does
nothing.

Doing so prevents prevents the unnecessary call to `visit-tags-table' function
and the subsequent `find-file' call for the `TAGS' file."
        ))

    (defun projectile-project-name ()
      "Return project name.
If the project root contains \"sos_\" or \"src\", remove the directory basename
from the project root name. E.g. if `projectile-project-root' is \"/a/b/src\",
remove the \"src\" portion from it and make it \"/a/b\"."
      (let ((project-root
             (condition-case nil
                 (projectile-project-root)
               (error default-directory)))
            project-name)
        (setq project-name (file-name-nondirectory
                            (directory-file-name project-root)))
        (while (string-match "\\(sos_\\|\\bsrc\\b\\)" project-name)
          (setq project-root (replace-regexp-in-string
                              "\\(.*\\)/.+/*$" "\\1" project-root))
          (setq project-name (file-name-nondirectory
                              (directory-file-name project-root))))
        project-name))

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

    ;; Use older version of `projectile-cache-current-file' that did not cache
    ;; the true names.
    ;; https://github.com/bbatsov/projectile/commit/924d73120bca6adc5b9bf3d79ad53075a63b5f1e
    (defun projectile-cache-current-file ()
      "Add the currently visited file to the cache."
      (interactive)
      (let* ((current-project (projectile-project-root))
             (abs-current-file (buffer-file-name (current-buffer)))
             (current-file (file-relative-name abs-current-file current-project)))
        (when (gethash (projectile-project-root) projectile-projects-cache)
          (unless (or (projectile-file-cached-p current-file current-project)
                      (projectile-ignored-directory-p (file-name-directory abs-current-file))
                      (projectile-ignored-file-p abs-current-file))
            (puthash current-project
                     (cons current-file (gethash current-project projectile-projects-cache))
                     projectile-projects-cache)
            (projectile-serialize-cache)
            (message "File %s added to project %s cache."
                     (propertize current-file 'face 'font-lock-keyword-face)
                     (propertize current-project 'face 'font-lock-keyword-face))))))

    (projectile-global-mode)))


(provide 'setup-projectile)

;; Do "touch ${PRJ_HOME}/.projectile" when updating a project. If the time stamp of
;; the ".projectile" is newer than that of the project cache then the existing
;; cache will be invalidated and recreated.
