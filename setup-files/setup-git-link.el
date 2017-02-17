;; Time-stamp: <2017-02-17 18:37:38 kmodi>

;; Git Link
;; https://github.com/sshaw/git-link

(use-package git-link
  :load-path "elisp/git-link"
  :bind (:map region-bindings-mode-map
         ("g" . modi/git-link))
  :init
  (progn
    (bind-to-modi-map "g" #'modi/git-link)
    (bind-to-modi-map "G" #'git-link-commit))
  :config
  (progn
    (defvar git-link-commit-fallback-use-latest-commit t
      "If non-nil, use the latest commit of the current file in the buffer if the
word under point is not a valid commit hash.")

    ;; Support emacs git links
    (defun git-link-savannah-gnu (hostname dirname filename branch commit start end)
      (format "http://%s/cgit/%s/tree/%s?id=%s#n%s"
              hostname
              (replace-regexp-in-string "^r/\\(.*\\)" "\\1.git" dirname)
              filename
              commit
              start))
    (add-to-list 'git-link-remote-alist '("git.savannah.gnu.org" git-link-savannah-gnu))

    (defun git-link-commit-savannah-gnu (hostname dirname commit)
      (format "http://%s/cgit/%s/commit/?id=%s"
              hostname
              (replace-regexp-in-string "^r/\\(.*\\)" "\\1.git" dirname)
              commit))
    (add-to-list 'git-link-commit-remote-alist '("git.savannah.gnu.org" git-link-commit-savannah-gnu))

    ;; Support org-mode git links
    (defun git-link-org-mode (hostname dirname filename branch commit start end)
      (format "http://%s/cgit.cgi/%s/tree/%s?id=%s#n%s"
              hostname
              (replace-regexp-in-string "^r/\\(.*\\)" "\\1.git" dirname)
              filename
              commit
              start))
    (add-to-list 'git-link-remote-alist '("orgmode.org" git-link-org-mode))

    (defun git-link-commit-org-mode (hostname dirname commit)
      (format "http://%s/cgit.cgi/%s/commit/?id=%s"
              hostname
              (replace-regexp-in-string "^r/\\(.*\\)" "\\1.git" dirname)
              commit))
    (add-to-list 'git-link-commit-remote-alist '("orgmode.org" git-link-commit-org-mode))

    (defun git-link-commit (remote)
      "Create a URL representing the commit for the hash under point
in the current buffer's GitHub/Bitbucket/GitLab/... repository. The URL will be
added to the kill ring.

If the word under point is not a valid commit hash and if
`git-link-commit-fallback-use-latest-commit' is non-nil, use the latest commit
hash of the file in the buffer.

With a prefix argument prompt for the remote's name. Defaults to \"origin\"."
      (interactive (list (git-link--select-remote)))
      (let* ((valid-commit-regexp "[a-z0-9]\\{7,40\\}")
             (remote-host (git-link--remote-host remote))
             (word-at-pt (word-at-point))
             (commit (if (and word-at-pt
                              (string-match valid-commit-regexp word-at-pt))
                         word-at-pt
                       (if git-link-commit-fallback-use-latest-commit
                           (git-link--last-commit)
                         "")))
             (handler (cl-loop for (key . value) in git-link-commit-remote-alist
                               when (string-match-p key remote-host)
                               finally return (car value))))
        (cond ((null remote-host)
               (message "Remote `%s' is unknown or contains an unsupported URL" remote))
              ((not (string-match valid-commit-regexp commit))
               (message "Invalid commit hash"))
              ((not (functionp handler))
               (message "No handler for %s" remote-host))
              ;; null ret val
              ((git-link--new
                (funcall handler
                         remote-host
                         (git-link--remote-dir remote)
                         commit))))))

    (defun modi/git-link (use-branch-maybe)
      "Get git link with the exact commit hash, not the branch name.
If USE-BRANCH-MAYBE is non-nil, use branch name in the link if available."
      (interactive "P")
      (let ((git-link-use-commit (if use-branch-maybe
                                     nil
                                   t))
            ;; Reset the current-prefix-arg, do not pass that to `git-link'.
            current-prefix-arg)
        (call-interactively #'git-link)))))


(provide 'setup-git-link)
