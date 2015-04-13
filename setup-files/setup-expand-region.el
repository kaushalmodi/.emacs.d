;; Time-stamp: <2015-04-13 15:13:18 kmodi>

;; Expand Region
;; https://github.com/magnars/expand-region.el

(use-package expand-region
  :config
  (progn
    ;; Patch to fix the issue caused by the `save-excursion' not saving the mark
    ;; any more in emacs 25.0+
    (>=e "25.0"
         (defun er--expand-region-1 ()
           "Increase selected region by semantic units.
Basically it runs all the mark-functions in `er/try-expand-list'
and chooses the one that increases the size of the region while
moving point or mark as little as possible."
           (let* ((p1 (point))
                  (p2 (if (use-region-p) (mark) (point)))
                  (start (min p1 p2))
                  (end (max p1 p2))
                  (try-list er/try-expand-list)
                  (best-start (point-min))
                  (best-end (point-max))
                  (set-mark-default-inactive nil))

             ;; add hook to clear history on buffer changes
             (unless er/history
               (add-hook 'after-change-functions 'er/clear-history t t))

             ;; remember the start and end points so we can contract later
             ;; unless we're already at maximum size
             (unless (and (= start best-start)
                          (= end best-end))
               (push (cons start end) er/history))

             (when (and expand-region-skip-whitespace
                        (er--point-is-surrounded-by-white-space)
                        (= start end))
               (skip-chars-forward er--space-str)
               (setq start (point)))

             (while try-list
               (save-excursion
                 (ignore-errors
                   (setq mark-active nil) ; Fix by Stefan Monnier
                   (funcall (car try-list))
                   (when (and (region-active-p)
                              (er--this-expansion-is-better start end best-start best-end))
                     (setq best-start (point))
                     (setq best-end (mark))
                     (when (and er--show-expansion-message (not (minibufferp)))
                       (message "%S" (car try-list))))))
               (setq try-list (cdr try-list)))

             (setq deactivate-mark nil)
             (goto-char best-start)
             (set-mark best-end)

             (er--copy-region-to-register)

             (when (and (= best-start (point-min))
                        (= best-end (point-max))) ;; We didn't find anything new, so exit early
               'early-exit))))

    (setq expand-region-contract-fast-key "|")
    (setq expand-region-reset-fast-key    "<ESC><ESC>")

    (bind-keys
     :map modi-mode-map
      ;; bind er/expand-region to `C-\' instead of `C-=' because `=' sign
      ;; clashes when trying to wrap a selection with `=' in org-mode using the
      ;; `wrap-region' package
      ("C-\\" . er/expand-region))))


(provide 'setup-expand-region)
