;; Time-stamp: <2015-02-23 11:42:32 kmodi>

;; Fill Column Indicator
;; Source: http://www.emacswiki.org/FillColumnIndicator

(use-package fill-column-indicator
  :config
  (progn

    (defvar modi/fci-mode-hooks '(verilog-mode-hook
                                  emacs-lisp-mode-hook
                                  python-mode-hook
                                  sh-mode-hook
                                  cperl-mode-hook
                                  org-mode-hook)
      "List of hooks of major modes in which fci mode should be enabled.")

    (defun modi/turn-on-fci-mode ()
      "Turn on fci-mode only for specific modes.
    As truncation is enabled only in fci-mode, truncation will be activated
    only in the below modes"
      (interactive)
      (dolist (hook modi/fci-mode-hooks)
        (add-hook hook #'fci-mode)))

    (defun modi/turn-off-fci-mode ()
      "Turn off fci-mode only for specific modes."
      (interactive)
      (dolist (hook modi/fci-mode-hooks)
        (remove-hook hook #'fci-mode)))

    (modi/turn-on-fci-mode)
    ;; Enable fci-mode automatically for all files
    ;; (add-hook 'after-change-major-mode-hook 'fci-mode)

    (setq fci-handle-truncate-lines t ;; Truncate lines in fci mode
          ;;fci-handle-truncate-lines nil  ;; Do not truncate lines in fci mode
          fci-rule-width 2
          fci-rule-use-dashes nil
          fci-dash-pattern 0.3)
    (setq-default fci-rule-column 80))) ;; default is 70


(provide 'setup-fci)
