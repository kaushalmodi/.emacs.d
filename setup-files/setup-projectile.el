;; Time-stamp: <2015-02-10 16:07:15 kmodi>

;; Projectile
;; Source: https://github.com/bbatsov/projectile

(req-package projectile
  :init
  (progn
    (setq projectile-ag-command (concat "\\ag " ; used unaliased version of `ag': \ag
                                        "-i " ; case insensitive
                                        "-f " ; follow symbolic links
                                        "--skip-vcs-ignores "
                                        ; Ignore files/dirs ONLY from `.agignore',
                                        ; NOT from vcs ignore files like .gitignore
                                        "-g ''" ; get file names matching the regex '',
                                        ; that is, ALL files not in ignore lists
                                        " | tr '\\n' '\\0'"
                                        ; Projectile needs null-character separated
                                        ; string (\0); ag returns newline (\n)
                                        ; separated list; So pipe the `ag' output
                                        ; to `tr' and replace all \n with \0.
                                        )))
  :config
  (progn
    (defun projectile-get-ext-command ()
      "Override the projectile-defined function so that `ag' is always used for
getting a list of all files in a project."
      projectile-ag-command)
    ;; Make the file list creation faster by NOT calling `projectile-get-sub-projects-files'
    (defun projectile-get-repo-files ()
      (projectile-files-via-ext-command (projectile-get-ext-command)))
    (add-to-list 'projectile-ignored-projects `,(concat (getenv "HOME") "/")) ; Don't consider my home dir as a project
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
    (setq projectile-mode-line " Prj")

    (defhydra hydra-projectile-other-window (:color blue)
      "projectile-other-window"
      ("b"  projectile-switch-to-buffer-other-window "buffer")
      ("d"  projectile-find-dir-other-window         "dir")
      ("f"  projectile-find-file-dwim-other-window   "file")
      ("q"  nil                                      nil :color blue))

    (bind-key "s-f"
              (defhydra hydra-projectile (:color blue)
                "projectile"
                ("a"   projectile-ag                      "ag")
                ("b"   projectile-switch-to-buffer        "buffer")
                ("d"   projectile-find-dir                "dir")
                ("s-f" projectile-find-file-dwim          "file")
                ("f"   projectile-find-file-dwim          nil)
                ("F"   projectile-find-file-in-directory  "Find file in dir")
                ("g"   ggtags-update-tags                 "gtags")
                ("s-g" ggtags-update-tags                 nil)
                ("i"   projectile-invalidate-cache        "invalidate")
                ("I"   projectile-ibuffer                 "Ibuffer")
                ("K"   projectile-kill-buffers            "Kill all buffers")
                ("s-k" projectile-kill-buffers            "Kill all buffers")
                ("m"   projectile-multi-occur             "multi-occur")
                ("o"   projectile-multi-occur             "multi-occur")
                ("s-p" projectile-switch-project          "switch")
                ("p"   projectile-switch-project          nil)
                ("s"   projectile-switch-project          nil)
                ("r"   projectile-recentf                 "recent")
                ("z"   projectile-cache-current-file      "cache current")
                ("`"   hydra-projectile-other-window/body "other window")
                ("q"   nil                                nil :color blue))
              modi-mode-map)


    ;; Globally enable Projectile
    (projectile-global-mode)))


(provide 'setup-projectile)

;; Do "touch ${PRJ_HOME}/.projectile" when updating a project. If the time stamp of
;; the ".projectile" is newer than that of the project cache then the existing
;; cache will be invalidated and recreated.
