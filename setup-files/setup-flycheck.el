;; Time-stamp: <2017-05-03 15:55:27 kmodi>

;; Flycheck
;; https://github.com/flycheck/flycheck

(use-package flycheck
  :defer t
  :config
  (progn
    (defconst modi/flycheck-mode-hooks '(emacs-lisp-mode-hook
                                         python-mode-hook
                                         sh-mode-hook
                                         nim-mode-hook)
      "List of hooks of major modes in which flycheck mode should be enabled.")

    (defun modi/turn-on-flycheck-mode ()
      "Turn on flycheck-mode for the modes in `modi/flycheck-mode-hooks'."
      (interactive)
      (dolist (hook modi/flycheck-mode-hooks)
        (add-hook hook #'flycheck-mode)))

    (defun modi/turn-off-flycheck-mode ()
      "Turn off flycheck-mode for the modes in `modi/flycheck-mode-hooks'."
      (interactive)
      (dolist (hook modi/flycheck-mode-hooks)
        (remove-hook hook #'flycheck-mode)))

    (modi/turn-on-flycheck-mode)))


(provide 'setup-flycheck)
