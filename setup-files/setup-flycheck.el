;; Time-stamp: <2025-02-11 13:53:23 kmodi>

;; Flycheck
;; https://github.com/flycheck/flycheck

(use-package flycheck
  :defer t
  :config
  (progn
    ;; Disable flake8 as flyspell checker as it keeps failing with:
    ;; Suspicious state from syntax checker python-flake8: Flycheck
    ;; checker python-flake8 returned 1, but its output contained no
    ;; errors: There was a critical error during execution of Flake8:
    ;; plugin code for `flake8-slots[SLOT]` does not match
    ;; ^[A-Z]{1,3}[0-9]{0,3}$
    (setq-default flycheck-disabled-checkers '(python-flake8))

    (defconst modi/flycheck-mode-hooks '(python-mode-hook
                                         sh-mode-hook
                                         ;; nim-mode-hook
                                         )
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
