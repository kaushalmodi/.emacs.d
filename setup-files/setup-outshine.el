;; Time-stamp: <2014-11-07 10:10:41 kmodi>

;; Outshine
;; https://github.com/tj64/outshine

;; (defvar outline-minor-mode-prefix "\M-#")
;; Above needs to be set using Customize so that it is set BEFORE the
;; `outline' (not `outshine') library is loaded.

(req-package outshine
  :config
  (progn
    (setq outshine-use-speed-commands t)
    (setq outshine-org-style-global-cycling-at-bob-p t)

    ;; Source: http://emacs.stackexchange.com/a/2803/115
    (defun modi/outshine-table-of-contents ()
      "Create a table of content for outshine headers
Insert/update the TOC after the line that has the \"// Contents:\" string.
Don't add \"Revision Control\" heading to TOC.
"
      (interactive)
      (save-excursion
        (goto-char (point-min))
        (let* ((headings-list nil)
               (stars-list nil)
               heading)
          (while (search-forward-regexp
                  "^// \\*\\(\\**\\) \\(.+\\)" nil :noerror)
            (setq star    (match-string 1))
            (setq heading (match-string 2))
            (message "%s %s" star heading)
            (when (not (string= heading "Revision Control"))
              (setq stars-list    (cons star stars-list))
              (setq headings-list (cons heading headings-list))))
          (setq stars-list    (nreverse stars-list))
          (setq headings-list (nreverse headings-list))

          (goto-char (point-min))
          (while (search-forward-regexp
                  "^// Contents:" nil :noerror)
            (forward-line 1)
            ;; First delete old contents
            ;; Keep on going on to the next line till it reaches a blank line
            (while (progn
                     (when (looking-at "^//")
                       (kill-line 1))
                     (not (looking-at "^\n"))))
            ;; Then print table of contents
            (insert "//\n")
            (let ((n 1))
              (dolist (h headings-list)
                ;; (insert (format "// %2d. %s\n" n heading))
                (insert (format "// %s%s\n" (replace-regexp-in-string "\\*" "  " (pop stars-list)) h))
                (setq n (1+ n))))))))

    (add-hook 'outline-minor-mode-hook #'outshine-hook-function)
    (dolist (hook '(verilog-mode-hook
                    matlab-mode-hook))
      (add-hook hook #'outline-minor-mode)
      (add-hook hook (λ (add-hook 'local-write-file-hooks #'modi/outshine-table-of-contents))))

    (bind-keys
     :map outline-minor-mode-map
     ("<M-up>"   . nil)
     ("M-p"      . outline-previous-visible-heading)
     ("<M-down>" . nil)
     ("M-n"      . outline-next-visible-heading))))


(provide 'setup-outshine)
