;; Time-stamp: <2016-07-14 15:37:44 kmodi>

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
    (setq git-link-commit-fallback-use-latest-commit t)

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
