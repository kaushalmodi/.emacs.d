;; -*- lexical-binding: t; -*-

;; Flymake
;; Python buffers are checked with pyflakes (see `python-flymake-command')
;; and shell scripts with shellcheck (see `sh-shellcheck-program').

(use-package flymake
  :defer t
  :init
  (progn
    (defconst modi/flymake-mode-hooks '(python-mode-hook
                                        python-ts-mode-hook
                                        sh-mode-hook
                                        bash-ts-mode-hook)
      "List of hooks of major modes in which flymake mode should be enabled.")

    (defun modi/turn-on-flymake-mode ()
      "Turn on flymake-mode for the modes in `modi/flymake-mode-hooks'."
      (interactive)
      (dolist (hook modi/flymake-mode-hooks)
        (add-hook hook #'flymake-mode)))

    (defun modi/turn-off-flymake-mode ()
      "Turn off flymake-mode for the modes in `modi/flymake-mode-hooks'."
      (interactive)
      (dolist (hook modi/flymake-mode-hooks)
        (remove-hook hook #'flymake-mode)))

    (modi/turn-on-flymake-mode)))


(provide 'setup-flymake)
