;; Time-stamp: <2018-08-22 18:29:34 kmodi>

;; Hugo
;; https://gohugo.io
;; https://github.com/kaushalmodi/ox-hugo

(use-package ox-hugo
  :load-path "elisp/ox-hugo"
  :after ox)

(use-package ox-hugo
  :commands (org-hugo-slug)
  :bind (:map modi-mode-map
         ("C-c G" . org-hugo-export-wim-to-md)))

(use-package ox-hugo-auto-export
  :load-path "elisp/ox-hugo")

(with-eval-after-load 'org-capture
  (defun org-hugo-new-subtree-post-capture-template ()
    "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
    (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
           (fname (org-hugo-slug title)))
      (mapconcat #'identity
                 `(
                   ,(concat "* TODO " title)
                   ":PROPERTIES:"
                   ,(concat ":EXPORT_HUGO_BUNDLE: " fname)
                   ":EXPORT_FILE_NAME: index"
                   ":EXPORT_HUGO_CUSTOM_FRONT_MATTER: :versions '()"
                   ":EXPORT_HUGO_CUSTOM_FRONT_MATTER+: :syndication '((mastodon . \"\") (twitter . \"\"))"
                   ":END:"
                   "%?\n")              ;Place the cursor here finally
                 "\n")))

  (add-to-list 'org-capture-templates
               '("s"                ;`org-capture' binding + s
                 "Hugo post for scripter.co"
                 entry
                 ;; It is assumed that below file is present in
                 ;; `org-directory' and that it has a "Blog Ideas" heading.
                 (file+olp "scripter-posts.org" "Blog Ideas")
                 (function org-hugo-new-subtree-post-capture-template))))


(provide 'setup-hugo)
