;; Time-stamp: <2016-02-28 00:21:34 kmodi>

;; Region Bindings Mode
;; https://github.com/fgallina/region-bindings-mode

;; Minor mode that enables the ability of having a custom keys for working with
;; regions. This is a pretty good way to keep the global bindings clean.

(use-package region-bindings-mode
  :config
  (progn
    (region-bindings-mode-enable)

    (defun modi/disable-rbm-deactivate-mark ()
      (interactive)
      (region-bindings-mode -1)
      (deactivate-mark)
      (message "Mark deactivated"))

    (bind-keys
     :map region-bindings-mode-map
      ("<C-SPC>" . modi/disable-rbm-deactivate-mark))))


(provide 'setup-region-bindings-mode)
