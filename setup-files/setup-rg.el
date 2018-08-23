;; Time-stamp: <2018-08-23 16:15:42 kmodi>

;; Deadgrep
;; https://github.com/Wilfred/deadgrep

(use-package deadgrep
  :ensure t
  :defer t
  :init
  (progn
    (bind-to-modi-map "r" #'deadgrep))
  :config
  (progn
    (defconst modi/deadgrep--rg-args
      `("--color=ansi"
        "--line-number"
        "--no-heading"
        "--with-filename"
        "--no-ignore-vcs"       ;Ignore files/dirs ONLY from `.ignore'
        "--follow"              ;Follow symlinks
        "%s"                    ;--fixed-strings and/or --word-regexp
        "%s"          ;--smart-case / --case-sensitive / --ignore-case
        "%s"          ;file type
        "%s"          ;context
        "--ignore-file" ,(expand-file-name ".ignore" (getenv "HOME")))
      "rg arguments used in the `deadgrep' package.")

    ;; Thu Aug 23 15:51:37 EDT 2018 - kmodi
    ;; Overriding the original `deadgrep--format-command' as I need to
    ;; add few extra `rg' options.
    ;; https://github.com/Wilfred/deadgrep/issues/24
    (defun modi/deadgrep--format-command (search-term search-type case context)
      "Return a command string that we can execute in a shell
to obtain ripgrep results."
      (let ((cmd (format
                  (mapconcat #'identity
                             (append '("%s")       ;`deadgrep-executable'
                                     modi/deadgrep--rg-args
                                     '("-- %s .")) ;search term
                             " ")
                  deadgrep-executable
                  (cond
                   ((eq search-type 'string)
                    "--fixed-strings")
                   ((eq search-type 'words)
                    "--fixed-strings --word-regexp")
                   ((eq search-type 'regexp)
                    "")
                   (t
                    (error "Unknown search type: %s" search-type)))
                  (cond
                   ((eq case 'smart)
                    "--smart-case")
                   ((eq case 'sensitive)
                    "--case-sensitive")
                   ((eq case 'ignore)
                    "--ignore-case")
                   (t
                    (error "Unknown case: %s" case)))
                  ;; TODO: pass this as an argument.
                  (cond
                   ((eq deadgrep--file-type 'all)
                    "")
                   ((eq (car-safe deadgrep--file-type) 'type)
                    (format "--type %s" (cdr deadgrep--file-type)))
                   ((eq (car-safe deadgrep--file-type) 'glob)
                    (format "--type-add 'custom:%s' --type custom"
                            (cdr deadgrep--file-type)))
                   (t
                    (error "Unknown file-type: %S" deadgrep--file-type)))
                  (if context
                      (format "--before-context %s --after-context %s"
                              (car context) (cdr context))
                    "")
                  (shell-quote-argument search-term))))
        ;; (message "%S" cmd)
        cmd))
    (advice-add 'deadgrep--format-command :override #'modi/deadgrep--format-command)))


(provide 'setup-rg)
