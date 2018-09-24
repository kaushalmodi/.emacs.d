;; Time-stamp: <2018-09-24 15:55:46 kmodi>

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

(with-eval-after-load 'org
  (defun modi/org-hugo-collapse-all-posts ()
    "Collapse all post subtrees in the current Org file.
Also collapse the Footnotes subtree and COMMENT subtrees if
present.

A post subtree is one that has the EXPORT_FILE_NAME property set."
    (interactive)
    (widen)
    (org-show-all '(headings))
    ;; Collapse all the post subtrees (ones with EXPORT_FILE_NAME
    ;; property set).
    (save-excursion
      (goto-char (point-min))
      (org-map-entries
       (lambda ()
         (hide-subtree))
       "EXPORT_FILE_NAME<>\"\""))
    ;; Also hide Footnotes and comments.
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\(\\* Footnotes\\|\\*+ COMMENT\\)" nil :noerror)
        (hide-subtree))))

  ;; C-u C-c TAB in Org mode -> `modi/org-hugo-collapse-all-posts'
  (defun modi/org-ctrl-c-tab-advice (&rest args)
    "Run `modi/org-hugo-collapse-all-posts' when doing \\[universal-argument] \\[org-ctrl-c-tab]."
    (let ((do-not-run-orig-fn (equal '(4) current-prefix-arg)))
      (when do-not-run-orig-fn
        (modi/org-hugo-collapse-all-posts))
      do-not-run-orig-fn))
  (advice-add 'org-ctrl-c-tab :before-until #'modi/org-ctrl-c-tab-advice))


(provide 'setup-hugo)
