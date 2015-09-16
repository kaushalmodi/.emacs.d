;; Time-stamp: <2015-09-16 17:09:38 kmodi>

;; Fill Column Indicator
;; http://www.emacswiki.org/FillColumnIndicator

(use-package fill-column-indicator
  :if (not (bound-and-true-p disable-pkg-fci))
  :config
  (progn
    ;; Set global default value for the local var `fci-handle-truncate-lines'
    (setq-default fci-handle-truncate-lines t) ; Truncate lines in fci mode
    ;; (setq-default fci-handle-truncate-lines nil) ; Do not truncate lines in fci mode

    (setq fci-rule-width 1)

    (with-eval-after-load 'setup-visual
      (setq fci-rule-column modi/fill-column)) ; default 70

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

    ;; Turn off fci-mode when popups are activated
    ;; https://github.com/alpaker/Fill-Column-Indicator/issues/21#issuecomment-6959718
    (with-eval-after-load 'popup
      (defvar sanityinc/fci-mode-suppressed nil)

      (defun sanityinc/suppress-fci-mode (&rest args)
        "Suspend fci-mode while popups are visible"
        (setq-local sanityinc/fci-mode-suppressed fci-mode)
        (when fci-mode
          (turn-off-fci-mode)))
      (advice-add 'popup-create :before #'sanityinc/suppress-fci-mode)

      (defun sanityinc/restore-fci-mode (&rest args)
        "Restore fci-mode when all popups have closed"
        (when (and sanityinc/fci-mode-suppressed
                   (null popup-instances))
          (setq-local sanityinc/fci-mode-suppressed nil)
          (turn-on-fci-mode)))
      (advice-add 'popup-delete :after #'sanityinc/restore-fci-mode))

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
