;; Time-stamp: <2017-07-21 17:52:53 kmodi>

;; Hugo
;; https://gohugo.io

(use-package ox-hugo
  :after ox
  :commands (org-hugo-export-subtree-to-md
             org-hugo-export-subtree-to-md-after-save
             org-hugo--slug)
  :load-path "elisp/ox-hugo"
  :bind (:map modi-mode-map
         ("C-c G" . org-hugo-export-subtree-to-md))) ;Same as "C-c C-e H H"

(with-eval-after-load 'org-capture
  (defun org-hugo-new-subtree-post-capture-template ()
    "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
    (let* ((hugo-date-fmt "%Y-%m-%dT%T%z")
           (date-nocolon (format-time-string hugo-date-fmt (current-time)))
           ;; Hugo expects the date stamp in this format:
           ;;   2017-07-06T14:59:45-04:00
           ;; But the "%Y-%m-%dT%T%z" format produces the date in this format:
           ;;   2017-07-06T14:59:45-0400 (Note the missing colon)
           ;; Below simply adds that colon.
           (date (replace-regexp-in-string "\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\'" "\\1:\\2" date-nocolon))
           (title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
           (fname (org-hugo--slug title)))
      (mapconcat #'identity
                 `(
                   ,(concat "* TODO " title)
                   ":PROPERTIES:"
                   ,(concat ":EXPORT_FILE_NAME: " fname)
                   ,(concat ":EXPORT_DATE: " date) ;Enter current date and time
                   ":END:"
                   "%?\n")          ;Place the cursor here finally
                 "\n")))

  (add-to-list 'org-capture-templates
               '("s"                ;`org-capture' binding + s
                 "Hugo post for scripter.co"
                 entry
                 ;; It is assumed that below file is present in
                 ;; `org-directory' and that it has a "Blog Ideas" heading.
                 (file+olp "scripter-posts.org" "Blog Ideas")
                 (function org-hugo-new-subtree-post-capture-template)))

  ;; Do not cause auto Org->Hugo export to happen when saving captures
  (defvar modi/org-to-hugo-export-on-save-enable nil
    "State variable to record if user has added
`org-hugo-export-subtree-to-md-after-save' to
`after-save-hook'.")

  (defun modi/org-capture--remove-auto-org-to-hugo-export-maybe ()
    "Function for `org-capture-before-finalize-hook'.
Disable `org-hugo-export-subtree-to-md-after-save'."
    (setq org-hugo-allow-export-after-save nil))
  (add-hook 'org-capture-before-finalize-hook
            #'modi/org-capture--remove-auto-org-to-hugo-export-maybe)

  (defun modi/org-capture--add-auto-org-to-hugo-export-maybe ()
    "Function for `org-capture-after-finalize-hook'.
Enable `org-hugo-export-subtree-to-md-after-save'."
    (setq org-hugo-allow-export-after-save t))
  (add-hook 'org-capture-after-finalize-hook
            #'modi/org-capture--add-auto-org-to-hugo-export-maybe))


(provide 'setup-hugo)
