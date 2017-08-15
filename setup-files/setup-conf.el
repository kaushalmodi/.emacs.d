;; Time-stamp: <2017-08-15 12:05:32 kmodi>

;; Conf Mode

(use-package conf-mode
  :mode (("\\.conf\\'" . conf-space-mode)
         ("\\.setup.*\\'" . conf-space-mode))
  :config
  (progn
    (defun modi/conf-quote-normal ()
      "Enable `conf-quote-normal' for *.setup files."
      (when-let* ((fname (buffer-file-name))
                  (enable-conf-quote-normal (string-match-p "\\.setup.*" fname)))
        ;; Set the syntax of ' and " to punctuation.
        (conf-quote-normal nil)))
    (add-hook 'conf-space-mode-hook #'modi/conf-quote-normal)))


(provide 'setup-conf)
