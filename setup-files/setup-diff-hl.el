;; Time-stamp: <2015-02-24 23:30:41 kmodi>

;; Diff-hl
;; https://github.com/dgutov/diff-hl

(use-package diff-hl
  :config
  (progn

    (defvar modi/diff-hl-mode-hooks '(emacs-lisp-mode-hook)
      "List of hooks of major modes in which diff-hl-mode should be enabled.")

    (dolist (hook modi/diff-hl-mode-hooks)
      (add-hook hook #'diff-hl-mode))

    (defhydra hydra-diff-hl (:color red)
      "diff-hl"
      ("=" diff-hl-diff-goto-hunk "goto hunk")
      ("n" diff-hl-revert-hunk    "revert hunk")
      ("[" diff-hl-previous-hunk  "prev hunk")
      ("]" diff-hl-next-hunk      "next hunk")
      ("q" nil                    "cancel"))
    (bind-key "s-v" #'hydra-diff-hl/body modi-mode-map)

    (add-hook 'dired-mode-hook #'diff-hl-dired-mode)))


(provide 'setup-diff-hl)
