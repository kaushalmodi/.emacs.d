;; Time-stamp: <2015-07-22 10:00:58 kmodi>

;; Indent Guide
;; https://github.com/zk-phi/indent-guide

(use-package indent-guide
  :config
  (progn
    (setq indent-guide-recursive t)
    (setq indent-guide-char "|")

    (defvar modi/indent-guide-mode-hooks '(verilog-mode-hook
                                           emacs-lisp-mode-hook
                                           python-mode-hook
                                           sh-mode-hook
                                           cperl-mode-hook)
      "List of hooks of major modes in which indent-guide-mode should be enabled.")

    (defun turn-on-indent-guide ()
      "Turn on indent-guide-mode only for specific modes."
      (interactive)
      (dolist (hook modi/indent-guide-mode-hooks)
        (add-hook hook #'indent-guide-mode)))

    (defun turn-off-indent-guide ()
      "Turn off indent-guide-mode only for specific modes."
      (interactive)
      (indent-guide-global-mode -1)
      (dolist (hook modi/indent-guide-mode-hooks)
        (remove-hook hook #'indent-guide-mode)))

    (indent-guide-global-mode -1)))


(provide 'setup-indent-guide)
