;; Time-stamp: <2015-02-17 12:01:30 kmodi>

;; Git Gutter(+), Git Gutter Fringe(+)

(defvar modi/git-gutter-original t
  "If set to `t', use the original `git-gutter' and `git-gutter-fringe'
packages. Else, use the \"plus\" versions.")

(defvar modi/git-gutter-mode-hooks '(emacs-lisp-mode-hook)
  "List of hooks of major modes in which git gutter(+) mode should be enabled.")

(if modi/git-gutter-original
    ;; Use `git-gutter' and `git-gutter-fringe'
    (progn
      (use-package git-gutter
        :config
        (progn
          (setq git-gutter:lighter " GitG")

          (defun modi/turn-on-git-gutter-mode ()
            "Turn on git-gutter-mode only for specific modes."
            (interactive)
            (dolist (hook modi/git-gutter-mode-hooks)
              (remove-hook hook #'git-gutter+-mode)
              (add-hook    hook #'git-gutter-mode)))

          (defun modi/turn-off-git-gutter-mode ()
            "Turn off git-gutter-mode only for specific modes."
            (interactive)
            (dolist (hook modi/git-gutter-mode-hooks)
              (remove-hook hook #'git-gutter-mode)))

          (use-package git-gutter-fringe)
          )))

  ;; Use `git-gutter+' and `git-gutter-fringe+'
  (progn
    (use-package git-gutter+
      :config
      (progn
        (setq git-gutter+-lighter " GitG+")

        (defun modi/turn-on-git-gutter-mode ()
          "Turn on git-gutter-mode only for specific modes."
          (interactive)
          (dolist (hook modi/git-gutter-mode-hooks)
            (remove-hook hook #'git-gutter-mode)
            (add-hook    hook #'git-gutter+-mode)))

        (defun modi/turn-off-git-gutter-mode ()
          "Turn off git-gutter-mode only for specific modes."
          (interactive)
          (dolist (hook modi/git-gutter-mode-hooks)
            (remove-hook hook #'git-gutter+-mode)))

        (use-package git-gutter-fringe+)
        ))))

(modi/turn-on-git-gutter-mode)


(provide 'setup-git-gutter)
