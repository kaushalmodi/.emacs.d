;; Time-stamp: <2018-04-19 00:48:43 kmodi>

;; Helper function `fontawesome-choose' used to uncomment only the
;; icons the user cares about in `fontawesome-all.js'.
;; https://scripter.co/optimize-your-fontawesome/

(defconst fontawesome-choose-icons '("list-alt" ;categories
                                     "tags"
                                     "rss"
                                     "link"
                                     "heart" ;like
                                     "reply"
                                     "retweet"
                                     "hacker-news"
                                     "github" ;"github-alt" "github-square"
                                     "twitter" ;"twitter-square"
                                     "gitlab")
  "List of icons to choose from fontawesome-all.js.
Used in `fontawesome-choose'.")

(defun fontawesome-choose ()
  "Comment out all icons in fontawesome-all.js except the selected few.

Minifying the resultant .js will then remove the commented icons,
thus drastically reducing the minified JS size.

Set the `fontawesome-choose-icons' variable to the list of icons
that you want to keep uncommented."
  (interactive)
  (let ((case-fold-search nil)
        (count 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^var icons" nil :noerror)
        (let ((begin (progn
                       (forward-line 1)
                       (point)))
              end)
          (re-search-forward "^\\};")
          (forward-line 0)
          (backward-char 1)
          (setq end (point))
          ;; First comment all the lines
          (save-excursion
            (narrow-to-region begin end)
            (goto-char (point-min))
            (while (re-search-forward "^\\s-*\\(\"\\)" nil :noerror)
              (replace-match "// \\1" nil nil nil 1))
            (widen))
          ;; Now uncomment only the selected icons
          (save-excursion
            (narrow-to-region begin end)
            (goto-char (point-min))
            (let* ((icon-regexp (regexp-opt fontawesome-choose-icons 'symbols))
                   (regexp (format "^\\s-*\\(//\\s-*\\)\"%s\":" icon-regexp)))
              (while (re-search-forward regexp nil :noerror)
                (replace-match "" nil nil nil 1)
                (setq count (1+ count))))
            (widen))))
      (message "fontawesome-choose: Uncommented %d icons matching %S" count fontawesome-choose-icons))))


(provide 'fontawesome-choose)
