;; Time-stamp: <2017-09-06 11:50:42 kmodi>

;; Handles the NEWS better

(defun modi/view-news-in-org-mode ()
  "Set the major mode to `org-mode' for NEWS files."
  (when (and buffer-file-name
             (string-match-p "/NEWS" buffer-file-name))
    (org-mode)
    ;; Do not truncate lines in NEWS files, otherwise `page-break-lines-mode'
    ;; acts strange. Also truncation is not needed in these files as they are
    ;; auto-filled.
    (toggle-truncate-lines 1)))
;; It is necessary to use the `find-file-hook' to set the major mode to
;; `org-mode' for NEWS files. The buffer-local variables in these files set the
;; major mode to `outline-mode'. So using `auto-mode-alist' to set the major
;; mode of NEWS files to `org-mode' will not work.
(add-hook 'find-file-hook #'modi/view-news-in-org-mode)

;; NEWS search
(when (executable-find "ag")
  (defun counsel-ag-news (&optional initial-input)
    "Search for a pattern in NEWS files using ag.
INITIAL-INPUT can be given as the initial minibuffer input."
    (interactive)
    (counsel-ag initial-input data-directory " -G '/NEWS'" "Search NEWS"))

  ;; Override the default binding to `view-emacs-news', which is also bound to
  ;; "C-h C-n" by default.
  (bind-key "C-h n" #'counsel-ag-news modi-mode-map))


(provide 'setup-news)
