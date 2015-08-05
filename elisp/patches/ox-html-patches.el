;; Time-stamp: <2015-08-05 09:20:41 kmodi>

;; `ox-html' needs to be required before `load'ing this patch file

(if (version<= (org-version) "8.2.99")
    ;; org-mode version 8.2
    (progn
      ;; Use "Code Snippet" string instead of "Listing" in HTML exports
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
                    "\n</ul>\n</div>\n</div>")))))
  ;; org-mode version 8.3+
  (progn
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
                  "\n</ul>\n</div>\n</div>"))))))
