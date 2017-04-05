;; Time-stamp: <2017-04-04 23:03:47 kmodi>

;; http://emacs.stackexchange.com/a/401/115

;; How to use this package:
;;
;;   ;; Execute `org-include-img-from-pdf' before saving the file.
;;   (defun my/org-include-img-from-pdf-before-save ()
;;     "Execute `org-include-img-from-pdf' just before saving the file."
;;     (add-hook 'before-save-hook #'org-include-img-from-pdf nil :local))
;;   (add-hook 'org-mode-hook #'my/org-include-img-from-pdf-before-save)
;;   ;; Execute `org-include-img-from-pdf' before exporting.
;;   (with-eval-after-load 'ox
;;     (add-hook 'org-export-before-processing-hook #'org-include-img-from-pdf))

;;;###autoload
(defun org-include-img-from-pdf (&rest _)
  "Convert pdf files to image files in org-mode bracket links.

    # ()convertfrompdf:t # This is a special comment; tells that the upcoming
                         # link points to the to-be-converted-to file.
    # If you have a foo.pdf that you need to convert to foo.png, use the
    # foo.png file name in the link.
    [[./foo.png]]
"
  (interactive)
  (if (executable-find "convert")
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^[ \t]*#\\s-+()convertfrompdf\\s-*:\\s-*t"
                                  nil :noerror)
          ;; Keep on going to the next line till it finds a line with bracketed
          ;; file link.
          (while (progn
                   (forward-line 1)
                   (not (looking-at org-bracket-link-regexp))))
          ;; Get the sub-group 1 match, the link, from `org-bracket-link-regexp'
          (let ((link (match-string-no-properties 1)))
            (when (stringp link)
              (let* ((imgfile (expand-file-name link))
                     (pdffile (expand-file-name
                               (concat (file-name-sans-extension imgfile)
                                       "." "pdf")))
                     (cmd (concat "convert -density 96 -quality 85 "
                                  pdffile " " imgfile)))
                (when (and (file-readable-p pdffile)
                           (file-newer-than-file-p pdffile imgfile))
                  ;; This block is executed only if pdffile is newer than
                  ;; imgfile or if imgfile does not exist.
                  (shell-command cmd)
                  (message "%s" cmd)))))))
    (user-error "`convert' executable (part of Imagemagick) is not found")))


(provide 'org-include-img-from-pdf)
