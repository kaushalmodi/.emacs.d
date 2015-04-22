;; Time-stamp: <2015-04-22 09:47:50 kmodi>

;; Diff-hl
;; https://github.com/sshaw/git-link

(use-package git-link
  :load-path "elisp/git-link"
  :config
  (progn
    (defun git-link-force-hash ()
      "This function is the same as `git-link' except that it copies the link
with the commit hash in the link even if branch name is available."
      (interactive)
      (let ((git-link-use-commit t))
        (call-interactively 'git-link)))
    (when (featurep 'region-bindings-mode)
      (bind-keys
       :map region-bindings-mode-map
        ("g" . git-link-force-hash)))))


(provide 'setup-git-link)
