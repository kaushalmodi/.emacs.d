;; Time-stamp: <2016-02-05 16:30:43 kmodi>

;; Git Link
;; https://github.com/sshaw/git-link

(use-package git-link
  :bind (:map region-bindings-mode-map
         ("g" . modi/git-link-force-hash))
  :config
  (progn
    (defun modi/git-link-force-hash ()
      "This function is the same as `git-link' except that it copies the link
with the commit hash in the link even if branch name is available."
      (interactive)
      (let ((git-link-use-commit t))
        (call-interactively 'git-link)))))


(provide 'setup-git-link)
