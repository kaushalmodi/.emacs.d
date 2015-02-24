;; Time-stamp: <2015-02-24 09:47:46 kmodi>

;; `ox-html' needs to be required before `load'ing this patch file

(defun org-html-fontify-code (code lang)
  "Color CODE with htmlize library.
      CODE is a string representing the source code to colorize.  LANG
      is the language used for CODE, as a string, or nil."
  (when code
    (cond
     ;; Case 1: No lang.  Possibly an example block.
     ((not lang)
      ;; Simple transcoding.
      (org-html-encode-plain-text code))
     ;; Case 2: No htmlize or an inferior version of htmlize
     ((not (and (require 'htmlize nil t) (fboundp 'htmlize-region-for-paste)))
      ;; Emit a warning.
      (message "Cannot fontify src block (htmlize.el >= 1.34 required)")
      ;; Simple transcoding.
      (org-html-encode-plain-text code))
     (t
      ;; Map language
      (setq lang (or (assoc-default lang org-src-lang-modes) lang))
      (let* ((lang-mode (and lang (intern (format "%s-mode" lang)))))
        (cond
         ;; Case 1: Language is not associated with any Emacs mode
         ((not (functionp lang-mode))
          ;; Simple transcoding.
          (org-html-encode-plain-text code))
         ;; Case 2: Default.  Fontify code.
         (t
          ;; htmlize
          (setq code (with-temp-buffer
                       ;; Switch to language-specific mode.
                       (funcall lang-mode)
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                       (when (require 'fill-column-indicator nil 'noerror)
                         (fci-mode -1))
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                       (insert code)
                       ;; Fontify buffer.
                       (font-lock-fontify-buffer)
                       ;; Remove formatting on newline characters.
                       (save-excursion
                         (let ((beg (point-min))
                               (end (point-max)))
                           (goto-char beg)
                           (while (progn (end-of-line) (< (point) end))
                             (put-text-property (point) (1+ (point)) 'face nil)
                             (forward-char 1))))
                       (org-src-mode)
                       (set-buffer-modified-p nil)
                       ;; Htmlize region.
                       (org-html-htmlize-region-for-paste
                        (point-min) (point-max))))
          ;; Strip any enclosing <pre></pre> tags.
          (let* ((beg (and (string-match "\\`<pre[^>]*>\n*" code) (match-end 0)))
                 (end (and beg (string-match "</pre>\\'" code))))
            (if (and beg end) (substring code beg end) code)))))))))

;; Remove HTML tags from the title string; otherwise the tags show up
;; verbatim in browser tabs3
(defun org-html--build-meta-info (info)
  "Return meta tags for exported document.
      INFO is a plist used as a communication channel."
  (let ((protect-string
         (lambda (str)
           (replace-regexp-in-string
            "\"" "&quot;" (org-html-encode-plain-text str))))
        (title (org-export-data (plist-get info :title) info))
        (author (and (plist-get info :with-author)
                     (let ((auth (plist-get info :author)))
                       (and auth
                            ;; Return raw Org syntax, skipping non
                            ;; exportable objects.
                            (org-element-interpret-data
                             (org-element-map auth
                                 (cons 'plain-text org-element-all-objects)
                               'identity info))))))
        (description (plist-get info :description))
        (keywords (plist-get info :keywords))
        (charset (or (and org-html-coding-system
                          (fboundp 'coding-system-get)
                          (coding-system-get org-html-coding-system
                                             'mime-charset))
                     "iso-8859-1")))
    (concat
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; (format "<title>%s</title>\n" title) ;; ORIGINAL
     ;; Remove HTML tags from `title' string
     (format "<title>%s</title>\n"
             (replace-regexp-in-string ".*\\(<.*>\\).*" ""
                                       title :fixedcase :literal 1))
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     (when (plist-get info :time-stamp-file)
       (format-time-string
        (concat "<!-- " org-html-metadata-timestamp-format " -->\n")))
     (format
      (if (org-html-html5-p info)
          (org-html-close-tag "meta" " charset=\"%s\"" info)
        (org-html-close-tag
         "meta" " http-equiv=\"Content-Type\" content=\"text/html;charset=%s\""
         info))
      charset) "\n"
      (org-html-close-tag "meta" " name=\"generator\" content=\"Org-mode\"" info)
      "\n"
      (and (org-string-nw-p author)
           (concat
            (org-html-close-tag "meta"
                                (format " name=\"author\" content=\"%s\""
                                        (funcall protect-string author))
                                info)
            "\n"))
      (and (org-string-nw-p description)
           (concat
            (org-html-close-tag "meta"
                                (format " name=\"description\" content=\"%s\"\n"
                                        (funcall protect-string description))
                                info)
            "\n"))
      (and (org-string-nw-p keywords)
           (concat
            (org-html-close-tag "meta"
                                (format " name=\"keywords\" content=\"%s\""
                                        (funcall protect-string keywords))
                                info)
            "\n")))))

;; Rename the use of "Listings" term in HTML exports
(defun org-html-list-of-listings (info)
  "Build a list of listings.
INFO is a plist used as a communication channel.  Return the list
of listings as a string, or nil if it is empty."
  (let ((lol-entries (org-export-collect-listings info)))
    (when lol-entries
      (concat "<div id=\"list-of-listings\">\n"
              (format "<h%d>%s</h%d>\n"
                      org-html-toplevel-hlevel
                      (org-html--translate "Code Snippets" info)
                      org-html-toplevel-hlevel)
              "<div id=\"text-list-of-listings\">\n<ul>\n"
              (let ((count 0)
                    (initial-fmt (format "<span class=\"listing-number\">%s</span>"
                                         (org-html--translate "Code Snippet %d:" info))))
                (mapconcat
                 (lambda (entry)
                   (let ((label (org-element-property :name entry))
                         (title (org-trim
                                 (org-export-data
                                  (or (org-export-get-caption entry t)
                                      (org-export-get-caption entry))
                                  info))))
                     (concat
                      "<li>"
                      (if (not label)
                          (concat (format initial-fmt (incf count)) " " title)
                        (format "<a href=\"#%s\">%s %s</a>"
                                (org-export-solidify-link-text label)
                                (format initial-fmt (incf count))
                                title))
                      "</li>")))
                 lol-entries "\n"))
              "\n</ul>\n</div>\n</div>"))))
