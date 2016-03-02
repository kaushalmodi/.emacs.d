;; Time-stamp: <2016-02-23 18:05:47 kmodi>

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
    ;; (setq projectile-mode-line '(:eval (format " Projectile[%s]" (projectile-project-name))))
    ;; (setq projectile-mode-line " ∏")
    ;; (setq projectile-mode-line " ℙ")
    (setq projectile-mode-line "​P")

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

    (defun modi/advice-projectile-dont-cache-ignored-projects (&rest args)
      "Do not cache files from ignored projects when doing `find-file'."
      (let ((do-not-cache (member (projectile-project-p)
                                  projectile-ignored-projects)))
        do-not-cache))
    (advice-add 'projectile-cache-files-find-file-hook :before-until
                #'modi/advice-projectile-dont-cache-ignored-projects)

    ;; Do not visit the current project's tags table if `ggtags-mode' is loaded.
    ;; Doing so prevents the unnecessary call to `visit-tags-table' function
    ;; and the subsequent `find-file' call for the `TAGS' file."
    (defun modi/advice-projectile-dont-visit-tags-table ()
      "Don't visit the tags table as we are using gtags/global."
      nil)
    (when (fboundp 'ggtags-mode)
      (advice-add 'projectile-visit-project-tags-table :override
                  #'modi/advice-projectile-dont-visit-tags-table))

    (defun modi/advice-projectile-project-root-no-truename ()
      "Retrieves the root directory of a project if available.
The current directory is assumed to be the project's root otherwise.

Do not try to get the truename in the case of symbolic links."
      (let ((dir default-directory))
        (or (--some (let* ((cache-key (format "%s-%s" it dir))
                           (cache-value (gethash cache-key projectile-project-root-cache)))
                      (if cache-value
                          cache-value
                        ;; (let ((value (funcall it (file-truename dir))))
                        (let ((value (funcall it dir)))
                          (puthash cache-key value projectile-project-root-cache)
                          value)))
                    projectile-project-root-files-functions)
            (if projectile-require-project-root
                (error "You're not in a project")
              default-directory))))
    (advice-add 'projectile-project-root :override
                #'modi/advice-projectile-project-root-no-truename)

    ;; https://github.com/bbatsov/projectile/commit/924d73120bca6adc5b9bf3d79ad53075a63b5f1e
    (defun modi/advice-projectile-cache-current-file-no-truename ()
      "Add the currently visited file to the cache, but do not try to find a
file's true name (don't follow symlinks)."
      (interactive)
      (let ((current-project (projectile-project-root)))
        (when (gethash (projectile-project-root) projectile-projects-cache)
          ;; (let* ((abs-current-file (file-truename (buffer-file-name (current-buffer))))
          (let* ((abs-current-file (buffer-file-name (current-buffer)))
                 (current-file (file-relative-name abs-current-file current-project)))
            (unless (or (projectile-file-cached-p current-file current-project)
                        (projectile-ignored-directory-p (file-name-directory abs-current-file))
                        (projectile-ignored-file-p abs-current-file))
              (puthash current-project
                       (cons current-file (gethash current-project projectile-projects-cache))
                       projectile-projects-cache)
              (projectile-serialize-cache)
              (message "File %s added to project %s cache."
                       (propertize current-file 'face 'font-lock-keyword-face)
                       (propertize current-project 'face 'font-lock-keyword-face)))))))
    (advice-add 'projectile-cache-current-file :override
                #'modi/advice-projectile-cache-current-file-no-truename)

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
      ("4"   hydra-projectile-other-window/body "other window")
      ("q"   nil "cancel" :color blue))

    (projectile-global-mode)))


(provide 'setup-projectile)

;; Do "touch ${PRJ_HOME}/.projectile" when updating a project. If the time stamp of
;; the ".projectile" is newer than that of the project cache then the existing
;; cache will be invalidated and recreated.
