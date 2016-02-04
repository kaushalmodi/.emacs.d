;; Time-stamp: <2016-02-04 11:10:19 kmodi>

;; Diff-hl
;; https://github.com/dgutov/diff-hl

(use-package diff-hl
  :config
  (progn

    (defvar modi/diff-hl-mode-hooks '(emacs-lisp-mode-hook
                                      sh-mode-hook)
      "List of hooks of major modes in which diff-hl-mode should be enabled.")

    (dolist (hook modi/diff-hl-mode-hooks)
      (add-hook hook #'diff-hl-mode))

    (defhydra hydra-diff-hl (:color red)
      "diff-hl"
      ("="     diff-hl-diff-goto-hunk "goto hunk")
      ("<RET>" diff-hl-diff-goto-hunk "goto hunk")
      ("u"     diff-hl-revert-hunk    "revert hunk")
      ("["     diff-hl-previous-hunk  "prev hunk")
      ("p"     diff-hl-previous-hunk  "prev hunk")
      ("]"     diff-hl-next-hunk      "next hunk")
      ("n"     diff-hl-next-hunk      "next hunk")
      ("q" nil                    "cancel"))
    (bind-key "s-v" #'hydra-diff-hl/body modi-mode-map)
    (bind-key "C-c v" #'hydra-diff-hl/body modi-mode-map)

    (add-hook 'dired-mode-hook #'diff-hl-dired-mode)))


(provide 'setup-diff)
