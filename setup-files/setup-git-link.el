;; Time-stamp: <2017-05-05 12:45:23 kmodi>

;; Git Link
;; https://github.com/sshaw/git-link

(use-package git-link
  :load-path "elisp/git-link"
  :commands (git-link)
  :bind (:map region-bindings-mode-map
         ("g" . modi/git-link))
  :init
  (progn
    (bind-to-modi-map "g" #'modi/git-link)
    (bind-to-modi-map "G" #'modi/git-link-commit-dwim))
  :config
  (progn
    ;; Support emacs git links
    (defun git-link-savannah-gnu (hostname dirname filename branch commit start end)
      (format "http://%s/cgit/%s/tree/%s?id=%s#n%s"
              hostname
              (replace-regexp-in-string "^r/\\(.*\\)" "\\1.git" dirname)
              filename
              commit
              start))
    (add-to-list 'git-link-remote-alist '("git\\.savannah\\.gnu\\.org" git-link-savannah-gnu))

    (defun git-link-commit-savannah-gnu (hostname dirname commit)
      (format "http://%s/cgit/%s/commit/?id=%s"
              hostname
              (replace-regexp-in-string "^r/\\(.*\\)" "\\1.git" dirname)
              commit))
    (add-to-list 'git-link-commit-remote-alist '("git\\.savannah\\.gnu\\.org" git-link-commit-savannah-gnu))

    ;; Support org-mode git links
    (defun git-link-org-mode (hostname dirname filename branch commit start end)
      (format "http://%s/cgit.cgi/%s/tree/%s?id=%s#n%s"
              hostname
              (replace-regexp-in-string "^r/\\(.*\\)" "\\1.git" dirname)
              filename
              commit
              start))
    (add-to-list 'git-link-remote-alist '("orgmode\\.org" git-link-org-mode))

    (defun git-link-commit-org-mode (hostname dirname commit)
      (format "http://%s/cgit.cgi/%s/commit/?id=%s"
              hostname
              (replace-regexp-in-string "^r/\\(.*\\)" "\\1.git" dirname)
              commit))
    (add-to-list 'git-link-commit-remote-alist '("orgmode\\.org" git-link-commit-org-mode))

    (defun modi/git-link-commit-dwim (remote)
      "Create a URL representing the commit for the hash under point
in the current buffer's GitHub/Bitbucket/GitLab/... repository. The URL will be
added to the kill ring.

If the word under point is not a valid commit hash, use the latest commit hash
of the file in the buffer.

With a prefix argument prompt for the remote's name. Defaults to \"origin\"."
      (interactive (list (git-link--select-remote)))
      ;; Replace the `word-at-point' function within `git-link-commit' with a
      ;; custom function that returns the latest commit hash of the file in
      ;; buffer if point is not on a valid commit hash.
      (cl-letf (((symbol-function 'word-at-point)
                 (lambda ()
                   (let ((word (thing-at-point 'word))
                         (valid-commit-regexp "[a-fA-F0-9]\\{7,40\\}"))
                     (if (and word (string-match-p valid-commit-regexp word))
                         word
                       (git-link--commit))))))
        (git-link-commit remote)))

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
