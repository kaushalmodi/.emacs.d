;; Time-stamp: <2015-02-24 09:43:19 kmodi>

;; Implementing Markdown style link IDs in org-mode
;; http://emacs.stackexchange.com/q/594/115

(org-add-link-type "linkid" #'endless/open-linkid-link #'endless/export-linkid-link)

(defun endless/open-linkid-link (path)
  "Follow an LINKID link to PATH."
  (browse-url (endless/find-linkid-link path)))

(defun endless/export-linkid-link (path desc format)
  "Create the export version of an LINKID link specified by LINK and DESC.
FORMATs understood are 'latex and 'html."
  (setq path (endless/find-linkid-link path))
  (cond
   ((eq format 'html)  (format "<a href=\"%s\">%s</a>" path desc))
   ((eq format 'latex) (format "\\href{%s}{%s}" path desc))
   ((eq format 'ascii) (format "[%s](%s)" desc path))
   (t desc)))

(defun endless/find-linkid-link (linkid &optional noerror)
  "Find \"#+LINK-ID: LINKID\" in current buffer and return the link.
Unless NOERROR is non-nil, throw an error if link not found."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (when (search-forward-regexp
             (format "^#\\+LINK-ID: \\b%s\\b +\\(.*\\) *$" linkid)
             nil noerror)
        (match-string-no-properties 1)))))

;; Patched `org-ascii-link' function from `ox-ascii.el'
;; Supports LINK TYPES added using `org-add-link-type' to ascii exports as well.
(require 'ox-ascii)
(setq org-ascii-links-to-notes nil)
(defun org-ascii-link (link desc info)
  "Transcode a LINK object from Org to ASCII.

DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information."
  (let* ((raw-link (org-element-property :raw-link link))
         (type (org-element-property :type link))
         (raw-path (replace-regexp-in-string
                    "%" "\\%" (org-element-property :path link) nil t))
         (path (cond
                ((member type '("http" "https" "ftp" "mailto"))
                 (concat type ":" raw-path))
                ((and (string= type "file") (file-name-absolute-p raw-path))
                 (concat "file:" raw-path))
                (t raw-path))))
    (cond
     ((string= type "coderef")
      (let ((ref (org-element-property :path link)))
	(format (org-export-get-coderef-format ref desc)
		(org-export-resolve-coderef ref info))))
     ;; Do not apply a special syntax on radio links.  Though, use
     ;; transcoded target's contents as output.
     ((string= type "radio") desc)
     ;; Do not apply a special syntax on fuzzy links pointing to
     ;; targets.
     ((string= type "fuzzy")
      (let ((destination (org-export-resolve-fuzzy-link link info)))
	(if (org-string-nw-p desc) desc
	  (when destination
	    (let ((number
		   (org-export-get-ordinal
		    destination info nil 'org-ascii--has-caption-p)))
	      (when number
		(if (atom number) (number-to-string number)
		  (mapconcat 'number-to-string number "."))))))))
     ;; Link type is handled by a special function.
     ((functionp (setq protocol (nth 2 (assoc type org-link-protocols))))
      (funcall protocol (org-link-unescape path) desc 'ascii))
     (t
      (if (not (org-string-nw-p desc))
          (format "[%s]" raw-link)
	(concat
	 (format "[%s]" desc)
	 (unless org-ascii-links-to-notes (format " (%s)" raw-link))))))))


(provide 'org-linkid)
