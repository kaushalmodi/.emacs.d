;; Time-stamp: <2015-09-29 11:42:59 kmodi>

;; Counsel (comes packaged with the `swiper' package)

(use-package counsel
  :if (not (bound-and-true-p disable-pkg-ivy))
  :config
  (progn
    (setq counsel-find-file-at-point t)
    (setq counsel-find-file-ignore-regexp
          (concat
           ;; file names beginning with # or .
           "\\(?:\\`[#.]\\)"
           ;; file names ending with # or ~
           "\\|\\(?:\\`.+?[#~]\\'\\)"))

    (ivy-set-actions
     'counsel-find-file
     `(("x"
        (lambda (x) (delete-file (expand-file-name x ivy--directory)))
        ,(propertize "delete" 'face 'font-lock-warning-face))))

    ;; Redefine `counsel-ag-function' with my required options, especially
    ;; the `--follow' option to allow search through symbolic links
    (defun counsel-ag-function (string &optional _pred &rest _unused)
      "Grep in the current directory for STRING."
      (if (< (length string) 3)
          (counsel-more-chars 3)
        (let ((regex (counsel-unquote-regex-parens
                      (setq ivy--old-re
                            (ivy--regex string)))))
          (counsel--async-command
           (format (concat "ag "
                           "--noheading "
                           "--nogroup "
                           "--nocolor "
                           "--skip-vcs-ignores "
                           "--smart-case "
                           "--follow " ; follow symlinks
                           "%S")
                   regex))
          nil)))

    (bind-keys
     :map modi-mode-map
      ("M-x"     . counsel-M-x)
      ("C-M-y"   . counsel-yank-pop)
      ("C-x C-f" . counsel-find-file)
      ("C-h v"   . counsel-describe-variable)
      ("C-h f"   . counsel-describe-function)
      ("C-h S"   . counsel-info-lookup-symbol)
      ("C-c u"   . counsel-unicode-char))

    (with-eval-after-load 'org
      (bind-key "C-c C-q" #'counsel-org-tag org-mode-map))
    (with-eval-after-load 'org-agenda
      (bind-key "C-c C-q" #'counsel-org-tag-agenda org-agenda-mode-map))

    (key-chord-define-global ";'" #'counsel-M-x)))


(provide 'setup-counsel)

;; Peek at files with `C-M-n' and `C-M-p'
;; Input a leading dot to see all files
;;
;; Related blog posts:
;; - http://oremacs.com/2015/06/08/describe-variable-tip
