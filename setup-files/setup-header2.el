;; Time-stamp: <2015-04-29 09:51:55 kmodi>

;; header2
;; http://www.emacswiki.org/emacs/header2.el

(use-package header2
  :config
  (progn

    (defconst modi/auto-headers-hooks '(verilog-mode-hook
                                        python-mode-hook
                                        sh-mode-hook
                                        cperl-mode-hook)
      "List of hooks of major modes in which headers should be auto-inserted.")

    (defun modi/turn-on-auto-headers ()
      "Turn on auto headers only for specific modes."
      (interactive)
      (dolist (hook modi/auto-headers-hooks)
        (add-hook hook #'auto-make-header)))

    (defun modi/turn-off-auto-headers ()
      "Turn off auto headers only for specific modes."
      (interactive)
      (dolist (hook modi/auto-headers-hooks)
        (remove-hook hook #'auto-make-header)))

    (defun modi/header-multiline ()
      "Insert multiline comment.  The comment text is in `header-multiline'."
      (let ((lineno  1)
            beg end nb-lines)
        (beginning-of-line)
        (if (nonempty-comment-end)
            (insert "\n" comment-start)
          ;; (header-blank)
          (insert header-prefix-string))
        (setq beg  (point))
        (insert header-multiline)
        (setq end       (point-marker)
              nb-lines  (count-lines beg end))
        (goto-char beg)
        (forward-line 1)
        (while (< lineno nb-lines)
          (insert header-prefix-string)
          (forward-line 1)
          (setq lineno  (1+ lineno)))
        (goto-char end)
        (when (nonempty-comment-end) (insert "\n"))
        (insert comment-end)
        (insert "\n")))

    (defsubst modi/header-timestamp ()
      "Insert field for timestamp"
      (insert header-prefix-string  "Time-stamp: <>\n"))

    (defsubst modi/header-projectname ()
      "Insert Project Name"
      (insert header-prefix-string "Project    : "
              (when (featurep 'projectile)
                (replace-regexp-in-string "/proj/\\(.*?\\)/.*"
                                          "\\1"
                                          (projectile-project-root)))
              "\n"))

    (defsubst modi/header-file-name ()
      "Insert \"Filename: \" line, using buffer's file name."
      (insert header-prefix-string "Filename   : "
              (if (buffer-file-name)
                  (file-name-nondirectory (buffer-file-name))
                (buffer-name))
              "\n"))

    (defsubst modi/header-author ()
      "Insert current user's name (`user-full-name') as this file's author."
      (insert header-prefix-string
              "Author     : "
              (user-full-name) "@"
              (replace-regexp-in-string ".*?\\(\\w+\\.\\w+\\)$" "\\1"
                                        (getenv "HOST"))
              "\n"))

    (defsubst modi/header-description ()
      "Insert \"Description: \" line."
      (insert header-prefix-string "Description: \n"))

    (defsubst modi/header-dash-line ()
      "Insert dashed line"
      (insert header-prefix-string)
      (insert-char ?- (- (if (featurep 'fill-column-indicator)
                             fci-rule-column
                           fill-column)
                         (current-column)))
      (insert "\n"))

    (defsubst modi/header-copyright ()
      "Insert a custom copyright block if available."
      (let ((header-multiline (or modi/custom-header-string
                                  header-copyright-notice)))
        (modi/header-multiline)))

    (defcustom modi/header-sos-revision-log-placeholder
      "* Revision Control
$Log: $"
      "Placeholder for insertion of revision logs auto inserted by SOS on check-in"
      :type 'string :group 'Automatic-File-Header)

    (defsubst modi/header-sos-revision-log ()
      "Insert placeholder for insertion of revision logs auto inserted by SOS on
check-in."
      (when (featurep 'sos-fns)
        (let ((header-multiline modi/header-sos-revision-log-placeholder))
          (dotimes (i 5) (insert "\n"))
          (modi/header-multiline))))

    (setq make-header-hook '(modi/header-timestamp
                             header-blank
                             modi/header-dash-line
                             modi/header-projectname
                             modi/header-file-name
                             modi/header-author
                             modi/header-description
                             modi/header-dash-line
                             modi/header-copyright
                             modi/header-dash-line
                             modi/header-sos-revision-log
                             header-eof))

    (modi/turn-on-auto-headers)))


(provide 'setup-header2)
