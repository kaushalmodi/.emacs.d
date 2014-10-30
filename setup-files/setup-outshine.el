;; Time-stamp: <2014-10-29 13:25:50 kmodi>

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
Don't add \"Revision\" heading to TOC.
"
      (interactive)
      (save-excursion
        (goto-char (point-min))
        (let* ((headings-found ()))
          (while (search-forward-regexp
                  "^// \\*+ \\(.+\\)"
                  nil :noerror)
            (when (not (string= (match-string 1) "Revision Control"))
              (setq headings-found (cons (match-string 1) headings-found))))

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
              (dolist (heading (reverse headings-found))
                (insert (format "// %2d. %s\n" n heading))
                (setq n (1+ n))))))))

    (add-hook 'outline-minor-mode-hook #'outshine-hook-function)
    (dolist (hook '(verilog-mode-hook
                    matlab-mode-hook))
      (add-hook hook #'outline-minor-mode)
      (add-hook hook (λ (add-hook 'local-write-file-hooks #'modi/outshine-table-of-contents))))))


(provide 'setup-outshine)
