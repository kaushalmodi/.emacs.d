;; Time-stamp: <2015-10-20 14:46:16 kmodi>

;; `ox-html' needs to be required before `load'ing this patch file

;; org-mode version 8.3+
;; `org-export-solidify-link-text' is deprecated
(defun org-html-list-of-listings (info)
  "Build a list of listings.
INFO is a plist used as a communication channel.  Return the list
of listings as a string, or nil if it is empty."
  (let ((lol-entries (org-export-collect-listings info)))
    (when lol-entries
      (concat "<div id=\"list-of-listings\">\n"
              (let ((top-level (plist-get info :html-toplevel-hlevel)))
                (format "<h%d>%s</h%d>\n"
                        top-level
                        (org-html--translate "Code Snippets" info)
                        top-level))
              "<div id=\"text-list-of-listings\">\n<ul>\n"
              (let ((count 0)
                    (initial-fmt (format "<span class=\"listing-number\">%s</span>"
                                         (org-html--translate "Code Snippet %d:" info))))
                (mapconcat
                 (lambda (entry)
                   (let ((label (and (org-element-property :name entry)
                                     (org-export-get-reference entry info)))
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
                                label
                                (format initial-fmt (incf count))
                                title))
                      "</li>")))
                 lol-entries "\n"))
              "\n</ul>\n</div>\n</div>"))))
