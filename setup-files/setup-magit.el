;; Time-stamp: <2015-06-17 13:02:01 kmodi>

;; magit
;; https://github.com/magit/magit

(use-package magit
  :commands (magit-status)
  :preface
  (progn
    (setq magit-last-seen-setup-instructions "1.4.0"))
  :config
  (progn
    (setq magit-completing-read-function #'magit-ido-completing-read)
    (setq magit-auto-revert-mode         nil)
    (setq magit-expand-staged-on-commit  nil) ; default = nil
    (setq magit-repo-dirs                `( ,user-emacs-directory))
    (setq magit-diff-options             nil) ; default
    ;; (setq magit-diff-options             '("--ignore-space-change"))

    (magit-auto-revert-mode -1)))


(provide 'setup-magit)

;; |---------+----------------------------------|
;; | Binding | Description                      |
;; |---------+----------------------------------|
;; | j n     | Jump to Untracked section        |
;; | j u     | Jump to Unstaged section         |
;; | j s     | Jump to Staged section           |
;; | j p     | Jump to Unpushed section         |
;; | M-p     | Jump to previous sibling section |
;; | M-n     | Jump to next sibling section     |
;; |---------+----------------------------------|

;; Tip: Adding prefix to above jump commands also expands those sections and
;; brings that section to the top of the buffer.
;;   So `C-u j s' is analogous to doing `j s C-l C-l 4`
