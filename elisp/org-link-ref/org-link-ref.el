;; Time-stamp: <2016-08-27 17:05:07 kmodi>

;; Implementing Markdown style link IDs in org-mode
;; http://emacs.stackexchange.com/q/594/115

(defun org-link-ref-find-link (link-ref)
  "Find link corresponding the LINK-REF in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (if (re-search-forward
           (concat "^#\\+LINK-REF:\\s-+"
                   link-ref
                   "\\s-+\\(.*?\\)\\s-*$") nil :noerror)
          (match-string-no-properties 1)
        (user-error "Definition for link reference `%s' was not found" link-ref)))))

(defun org-link-ref-follow-link (link-ref)
  "Browse the link pointed by LINK-REF."
  (browse-url (org-link-ref-find-link link-ref)))

(defun org-link-ref-export-link (link-ref desc format)
  "Create export version of LINK-REF and DESC to FORMAT.
FORMATs understood are `html', `latex' and `ascii'."
  (let ((link (org-link-ref-find-link link-ref)))
    (cond
     ((eq format 'html)  (format "<a href=\"%s\">%s</a>" link desc))
     ((eq format 'latex) (format "\\href{%s}{%s}" link desc))
     ((eq format 'ascii) (format "[%s](%s)" desc link))
     (t desc))))

(if (fboundp 'org-link-set-parameters)  ; org 9.x
    (org-link-set-parameters "link-ref"
                             :follow #'org-link-ref-follow-link
                             :export #'org-link-ref-export-link)
  (org-add-link-type "link-ref"         ; org 8.x and older
                     #'org-link-ref/follow-link
                     #'org-link-ref/export-link))


(provide 'org-link-ref)
