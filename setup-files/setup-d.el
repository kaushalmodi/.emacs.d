;; Time-stamp: <2015-11-04 11:45:04 kmodi>

;; D
;; https://github.com/Emacs-D-Mode-Maintainers/Emacs-D-Mode

(use-package d-mode
  :ensure t
  :mode (("\\.d\\'" . d-mode))
  :config
  (progn
    ;; Use my default bindings for `C-d' and `backspace'
    (bind-keys
     :map d-mode-map
      ("C-d" . nil)
      ("<DEL>" . nil))

    (defun modi/d-mode-stuff ()
      (setq-local compile-command (concat "dmd -de -w -unittest "
                                          buffer-file-name)))
    (add-hook 'd-mode-hook #'modi/d-mode-stuff)))


(provide 'setup-d)
