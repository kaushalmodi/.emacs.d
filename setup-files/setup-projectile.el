;; Time-stamp: <2014-10-15 16:53:08 kmodi>

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
                                        ))
    (defun projectile-get-ext-command ()
      "Override the projectile-defined function so that `ag' is always used for
getting a list of all files in a project."
      projectile-ag-command)
    ;; (setq projectile-enable-caching nil)
    (setq projectile-enable-caching t) ; Enable caching, otherwise
                                        ; `projectile-find-file' is really slow
                                        ; for large projects
    (add-to-list 'projectile-project-root-files-bottom-up ".SOS")
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
