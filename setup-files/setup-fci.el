;; Time-stamp: <2015-04-01 09:37:03 kmodi>

;; Fill Column Indicator
;; http://www.emacswiki.org/FillColumnIndicator

(use-package fill-column-indicator
    :config
  (progn

    (defconst modi/fci-mode-hooks '(verilog-mode-hook
                                    emacs-lisp-mode-hook
                                    python-mode-hook
                                    sh-mode-hook
                                    ;; org-src-mode-hook
                                    cperl-mode-hook)
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
    ;; (add-hook 'after-change-major-mode-hook #'fci-mode)

    (setq-default fci-handle-truncate-lines t) ; Truncate lines in fci mode
    ;; (setq-default fci-handle-truncate-lines nil) ; Do not truncate lines in fci mode
    (setq-default fci-rule-width 1)
    (setq-default fci-rule-use-dashes nil)
    (setq-default fci-dash-pattern 0.3)
    (setq-default fci-rule-column 80) ; default is 70

    (defun modi/fci-redraw-frame-all-buffers ()
      "Redraw the fill-column rule in all buffers on the selected frame.
Running this function after changing themes updates the fci rule color in
all the buffers."
      (interactive)
      (let ((bufs (delete-dups (buffer-list (selected-frame)))))
        (dolist (buf bufs)
          (with-current-buffer buf
            (when fci-mode
              (fci-delete-unneeded)
              (fci-make-overlay-strings)
              (fci-update-all-windows t))))))))


(provide 'setup-fci)
