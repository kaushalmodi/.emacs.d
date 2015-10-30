;; Time-stamp: <2015-10-30 00:01:42 kmodi>

;; D
;; https://github.com/Emacs-D-Mode-Maintainers/Emacs-D-Mode

(use-package d-mode
  :ensure t
  :mode (("\\.d\\'" . d-mode))
  :config
  (progn
    ;; Use my default bindings for `C-d' and `backspace'
    (define-key d-mode-map (kbd "C-d") nil)
    (define-key d-mode-map (kbd "<DEL>") nil)

    (defun modi/d-mode-stuff ()
      (setq-local compile-command (concat "dmd -de -w -unittest "
                                          buffer-file-name)))
    (add-hook 'd-mode-hook #'modi/d-mode-stuff)))


(provide 'setup-d)
