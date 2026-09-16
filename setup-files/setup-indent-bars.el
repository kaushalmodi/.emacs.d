;; -*- lexical-binding: t; -*-

;; Indent Bars
;; https://github.com/jdtsmith/indent-bars

(use-package indent-bars
  :commands (modi/turn-on-indent-bars)
  :config
  (progn
    ;; Use tree-sitter scope information when the buffer has a parser.
    (setq indent-bars-treesit-support t)

    (defvar modi/indent-bars-mode-hooks '(verilog-mode-hook
                                          emacs-lisp-mode-hook
                                          python-mode-hook
                                          python-ts-mode-hook
                                          sh-mode-hook
                                          bash-ts-mode-hook
                                          cperl-mode-hook)
      "List of hooks of major modes in which `indent-bars-mode' should be enabled.")

    (defun modi/turn-on-indent-bars ()
      "Turn on `indent-bars-mode' only for specific modes."
      (interactive)
      (dolist (hook modi/indent-bars-mode-hooks)
        (add-hook hook #'indent-bars-mode)))

    (defun modi/turn-off-indent-bars ()
      "Turn off `indent-bars-mode' only for specific modes."
      (interactive)
      (dolist (hook modi/indent-bars-mode-hooks)
        (remove-hook hook #'indent-bars-mode)))))


(provide 'setup-indent-bars)
