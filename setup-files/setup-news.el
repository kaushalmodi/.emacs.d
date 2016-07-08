;; Time-stamp: <2016-07-08 12:48:22 kmodi>

;; Handles the NEWS better

(defun modi/view-news-in-org-mode ()
  "Set the major mode to `org-mode' for NEWS files."
  (when (string-match-p "/NEWS" buffer-file-name)
    (org-mode)
    ;; Do not truncate lines in NEWS files, otherwise `page-break-lines-mode'
    ;; acts strange. Also truncation is not needed in these files as they
    ;; are auto-filled.
    (toggle-truncate-lines 1)))
;; It is necessary to use the `find-file-hook' to set the major mode to
;; `org-mode' for NEWS files. The buffer-local variables in these files set the
;; major mode to `outline-mode'. So using `auto-mode-alist' to set the major
;; mode of NEWS files to `org-mode' will not work.
(add-hook 'find-file-hook #'modi/view-news-in-org-mode)

;; NEWS search
(when (executable-find "ag")
  (defun counsel-news-function (regexp)
    "Search in all NEWS files for REGEXP."
    (if (< (length regexp) 3)
        (counsel-more-chars 3)
      (let ((default-directory data-directory)
            (regex (counsel-unquote-regex-parens
                    (setq ivy--old-re (ivy--regex regexp)))))
        (counsel--async-command
         (format (concat counsel-ag-base-command
                         ;; search only in files whose names contain '/NEWS'
                         " -G '/NEWS'")
                 (shell-quote-argument regex)))
        nil)))

  (defun counsel-news (&optional initial-input)
    "Grep for a pattern in regr*list files using ag.
INITIAL-INPUT can be given as the initial minibuffer input."
    (interactive)
    (require 'counsel)
    (setq counsel--git-grep-dir data-directory)
    (ivy-read "Search NEWS: " 'counsel-news-function
              :initial-input initial-input
              :dynamic-collection t
              :history 'counsel-git-grep-history
              :action #'counsel-git-grep-action
              :unwind (lambda ()
                        (counsel-delete-process)
                        (swiper--cleanup))
              :caller 'counsel-news))

  ;; Override the default binding to `view-emacs-news', which is also bound to
  ;; "C-h C-n" by default.
  (bind-key "C-h n" #'counsel-news modi-mode-map))


(provide 'setup-news)
