;; Time-stamp: <2016-05-19 22:24:54 kmodi>

;; Git Link
;; https://github.com/sshaw/git-link

(use-package git-link
  :load-path "elisp/git-link"
  :bind (:map region-bindings-mode-map
         ("g" . modi/git-link-force-hash))
  :init
  (progn
    (bind-to-modi-map "g" #'git-link-commit))
  :config
  (progn
    (setq git-link-commit-fallback-use-latest-commit t)

    (defun modi/git-link-force-hash ()
      "This function is the same as `git-link' except that it copies the link
with the commit hash in the link even if branch name is available."
      (interactive)
      (let ((git-link-use-commit t))
        (call-interactively #'git-link)))))


(provide 'setup-git-link)
