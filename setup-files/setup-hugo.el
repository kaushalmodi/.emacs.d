;; Time-stamp: <2017-09-08 13:50:17 kmodi>

;; Hugo
;; https://gohugo.io
;; https://github.com/kaushalmodi/ox-hugo

(use-package ox-hugo
  :after ox
  :commands (org-hugo-export-subtree-to-md
             org-hugo-export-subtree-to-md-after-save
             org-hugo-slug)
  :load-path "elisp/ox-hugo"
  :bind (:map modi-mode-map
         ("C-c G" . org-hugo-export-subtree-to-md)) ;Same as "C-c C-e H H"
  :config
  (progn
    ;; Set `org-hugo-langs-no-descr-in-code-fences' to '(org) if setting
    ;; `pygmentsCodeFences' to `true' in Hugo site `config.toml', as syntax
    ;; highlighting for Org code blocks is not yet supported by Pygments:
    ;; https://github.com/kaushalmodi/ox-hugo/issues/60
    (setq org-hugo-langs-no-descr-in-code-fences '(org))))

(with-eval-after-load 'org-capture
  (defun org-hugo-new-subtree-post-capture-template ()
    "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
    (let* (;; http://www.holgerschurig.de/en/emacs-blog-from-org-to-hugo/
           (date (format-time-string (org-time-stamp-format :long :inactive) (org-current-time)))
           (title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
           (fname (org-hugo-slug title)))
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
  (defun modi/org-capture--remove-auto-org-to-hugo-export-maybe ()
    "Function for `org-capture-before-finalize-hook'.
Disable `org-hugo-export-subtree-to-md-after-save'."
    (setq org-hugo-allow-export-after-save nil))

  (defun modi/org-capture--add-auto-org-to-hugo-export-maybe ()
    "Function for `org-capture-after-finalize-hook'.
Enable `org-hugo-export-subtree-to-md-after-save'."
    (setq org-hugo-allow-export-after-save t))

  (add-hook 'org-capture-before-finalize-hook #'modi/org-capture--remove-auto-org-to-hugo-export-maybe)
  (add-hook 'org-capture-after-finalize-hook #'modi/org-capture--add-auto-org-to-hugo-export-maybe))


(provide 'setup-hugo)
