;; Time-stamp: <2015-02-02 10:34:33 kmodi>

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

    (bind-keys
     :map modi-mode-map
     ("s-f s-f" . projectile-find-file)) ;; Win-f Win-f

    ;; Globally enable Projectile
    (projectile-global-mode)))


(provide 'setup-projectile)
