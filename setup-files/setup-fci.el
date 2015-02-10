;; Time-stamp: <2015-02-10 11:12:03 kmodi>

;; Fill Column Indicator
;; Source: http://www.emacswiki.org/FillColumnIndicator

(req-package fill-column-indicator
  :defer t
  :config
  (progn

    (defun modi/turn-on-fci-mode ()
      "Turn on fci-mode only for specific modes.
    As truncation is enabled only in fci-mode, truncation will be activated
    only in the below modes"
      (interactive)
      (dolist (hook '(verilog-mode-hook
                      emacs-lisp-mode-hook
                      python-mode-hook
                      sh-mode-hook
                      cperl-mode-hook
                      org-mode-hook))
        (add-hook hook 'fci-mode)))

    (defun modi/turn-off-fci-mode ()
      "Turn off fci-mode only for specific modes."
      (interactive)
      (dolist (hook '(verilog-mode-hook
                      emacs-lisp-mode-hook
                      python-mode-hook
                      sh-mode-hook
                      cperl-mode-hook
                      org-mode-hook))
        (remove-hook hook 'fci-mode)))
    (modi/turn-on-fci-mode)
    ;; Enable fci-mode automatically for all files
    ;; (add-hook 'after-change-major-mode-hook 'fci-mode)

    (setq fci-handle-truncate-lines t ;; Truncate lines in fci mode
          ;;fci-handle-truncate-lines nil  ;; Do not truncate lines in fci mode
          fci-rule-width 2
          fci-rule-use-dashes nil
          fci-dash-pattern 0.3)
    (setq-default fci-rule-column 80))) ;; default is 70


(setq setup-fci-loaded t) ;; required.. used in setup-visual
(provide 'setup-fci)
