;; Time-stamp: <2015-03-05 11:23:45 kmodi>

;; magit
;; https://github.com/magit/magit

(use-package magit
    :commands (magit-status)
    :init
    (progn
      (bind-keys
       :map modi-mode-map
       ("<f7>"   . magit-status)
       ("<S-f7>" . magit-push)
       ("<M-f7>" . magit-pull))
      (when (featurep 'key-chord)
        (key-chord-define-global "-[" 'magit-status))) ; alternative to F11
    :config
    (progn
      (setq magit-completing-read-function #'magit-ido-completing-read)
      (setq magit-auto-revert-mode         nil)
      (setq magit-expand-staged-on-commit  nil) ; default = nil
      (setq magit-repo-dirs                `( ,user-emacs-directory))
      (setq magit-diff-options             '("--ignore-space-change"))

      (magit-auto-revert-mode -1) ; Disable magit auto revert

      ;; http://emacs.stackexchange.com/a/2195/115
      ;; While in "*magit..*" buffer on doing Commit (`c c'), the
      ;; ".. COMMIT_EDITMSG" buffer opens and reuses the "*magit .." window.
      ;; This is not useful when you'd want to add details about what you are
      ;; committing while reviewing the diff in "*magit .." window. So ensure
      ;; that the ".. COMMIT_EDITMSG" buffer always pops up in a new window.
      ;; `display-buffer-alist' = '(CONDITION . (FUNCTION . ALIST))
      (add-to-list 'display-buffer-alist
                   '(".*COMMIT_EDITMSG". ((display-buffer-pop-up-window) .
                                          ((inhibit-same-window . t)))))))


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
