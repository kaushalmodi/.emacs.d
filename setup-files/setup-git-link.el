;; Time-stamp: <2016-02-17 11:00:18 kmodi>

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
        (call-interactively 'git-link)))

    ;; Add support for git.savannah.gnu.org
    (add-to-list 'git-link-remote-alist '("git.savannah.gnu.org" git-link-savannah-gnu))
    (add-to-list 'git-link-commit-remote-alist '("git.savannah.gnu.org" git-link-commit-savannah-gnu))

    (defun git-link-savannah-gnu (hostname dirname filename branch commit start end)
      ;; Example: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/README?id=d4b93e11a519cf71beca69654fda158d01a26c3b#n1
      (format "http://%s/cgit/%s/tree/%s?id=%s%s"
              hostname
              (replace-regexp-in-string "^r/\\(.*\\)" "\\1.git" dirname)
              filename
              (or branch commit)
              (format "#n%s" start)))

    (defun git-link-commit-savannah-gnu (hostname dirname commit)
      ;; Example: http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=2abcb06cab03cf9040348146fcc0e3e93ae24a58
      (format "http://%s/cgit/%s/commit/?id=%s"
              hostname
              (replace-regexp-in-string "^r/\\(.*\\)" "\\1.git" dirname)
              commit))))


(provide 'setup-git-link)
