;; Time-stamp: <2016-09-27 22:13:54 kmodi>

;; Region Bindings Mode
;; https://github.com/fgallina/region-bindings-mode

;; Minor mode that enables the ability of having a custom keys for working with
;; regions. This is a pretty good way to keep the global bindings clean.

(use-package region-bindings-mode
  :config
  (progn
    ;; Do not activate `region-bindings-mode' in Special modes like `dired' and
    ;; `ibuffer'. Single-key bindings like 'm' are useful in those modes even
    ;; when a region is selected.
    (setq region-bindings-mode-disabled-modes '(dired-mode
                                                ibuffer-mode))

    (region-bindings-mode-enable)

    (defun modi/disable-rbm-deactivate-mark ()
      "Disable `region-bindings-mode' and deactivate mark."
      (interactive)
      (region-bindings-mode -1)
      (deactivate-mark)
      (message "Mark deactivated"))

    (bind-keys
     :map region-bindings-mode-map
      ("<C-SPC>" . modi/disable-rbm-deactivate-mark))))


(provide 'setup-region-bindings-mode)
