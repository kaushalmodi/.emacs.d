;; Time-stamp: <2022-03-01 11:48:05 kmodi>

;; Hugo
;; https://gohugo.io
;; https://github.com/kaushalmodi/ox-hugo

(defvar modi/ox-hugo-dir (file-name-as-directory (expand-file-name "elisp/ox-hugo" user-emacs-directory))
  "Directory containing ox-hugo package.")

(defvar modi/ox-hugo-autoloads-file (expand-file-name "ox-hugo-autoloads.el" modi/ox-hugo-dir)
  "Path to ox-hugo package's generated autoloads file.")

;; Below is needed so that the "put .. safe-local-variable" forms get
;; evaluated from the ox-hugo's autoloads file.
(unless (file-exists-p modi/ox-hugo-autoloads-file)
  (let ((generated-autoload-file modi/ox-hugo-autoloads-file))
    (update-directory-autoloads modi/ox-hugo-dir)))
(load-file modi/ox-hugo-autoloads-file)

(use-package ox-hugo
  :load-path modi/ox-hugo-dir
  :commands (org-hugo-slug)
  :bind (:map modi-mode-map
         ("C-c G" . org-hugo-export-wim-to-md)))

(use-package ox-hugo
  :load-path modi/ox-hugo-dir
  :after ox
  :config
  (progn
    (add-to-list 'org-hugo-external-file-extensions-allowed-for-copying "csv")
    (add-to-list 'org-hugo-external-file-extensions-allowed-for-copying "vplanx")

    (add-to-list 'org-hugo-special-block-type-properties '("sidenote" . (:trim-pre t :trim-post t)))

    (defun modi/org-hugo-inline-src-block (inline-src-block _contents _info)
      "Transcode INLINE-SRC-BLOCK object into Hugo-compatible Markdown format.

This advice override will work given that the below code snippet is saved as `inline-src.html' in
the Hugo site's \"layouts/shortcodes/\" directory:

    {{- transform.Highlight .Inner (.Get 0) (.Get 1 | default \"\")
        | replaceRE \\=`^<div class=\"highlight\"><pre [^>]+>((.|\\n)+)</pre></div>$\\=`
                    \\=`<span class=\"inline-src chroma\">${1}</span>\\=`
        | safeHTML -}}"
      (let* ((lang (org-element-property :language inline-src-block))
             (code (org-hugo--escape-hugo-shortcode
                    (org-element-property :value inline-src-block)
                    lang)))
        (format "{{< inline-src %s >}}%s{{< /inline-src >}}" lang code)))
    (advice-add 'org-hugo-inline-src-block :override #'modi/org-hugo-inline-src-block)))

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

Also collapse:
- The Footnotes subtree and COMMENT subtrees if present.
- Subtrees that have the CUSTOM_ID property set.

A post subtree is one that has the EXPORT_FILE_NAME property
set."
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
       "EXPORT_FILE_NAME<>\"\"|CUSTOM_ID<>\"\""))
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
