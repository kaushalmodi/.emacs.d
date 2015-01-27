;; Time-stamp: <2015-01-27 13:53:20 kmodi>

;; magit
;; Source: https://github.com/magit/magit

(req-package magit
  :commands (magit-status)
  :requires (key-chord)
  :init
  (progn
    (bind-keys
     :map modi-mode-map
     ("<f11>"   . magit-status)
     ("<S-f11>" . magit-push)
     ("<M-f11>" . magit-pull))
    (key-chord-define-global "-[" 'magit-status)) ;; alternative for F11
  :config
  (progn
    (setq magit-completing-read-function 'magit-ido-completing-read)
    (setq magit-auto-revert-mode nil)
    (setq magit-expand-staged-on-commit nil) ; default = nil
    (setq magit-repo-dirs '( "~/.emacs.d"))
    (setq magit-diff-options '("--ignore-space-change"))
    (magit-auto-revert-mode -1) ;; Disable magit auto revert
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
