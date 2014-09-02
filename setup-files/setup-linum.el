;; Time-stamp: <2014-08-13 12:59:01 kmodi>

;; NLinum

(req-package nlinum
  :require (verilog-mode cperl-mode python-mode)
  :config
  (progn
    (setq global-nlinum-mode -1
          nlinum-format      "%4d ") ; right aligned, 4 char wide line num col

    (defun turn-on-nlinum ()
      "Turn on nlinum mode in specific modes."
      (interactive)
      (dolist (hook '(verilog-mode-hook
                      emacs-lisp-mode-hook
                      cperl-mode-hook
                      c-mode-hook
                      python-mode-hook
                      matlab-mode-hook
                      sh-mode-hook
                      web-mode-hook
                      html-mode-hook
                      css-mode-hook
                      makefile-gmake-mode-hook))
        (add-hook hook 'nlinum-mode)))

    (defun turn-off-nlinum ()
      "Unhook nlinum mode from various major modes."
      (interactive)
      (dolist (hook '(verilog-mode-hook
                      emacs-lisp-mode-hook
                      cperl-mode-hook
                      c-mode-hook
                      python-mode-hook
                      matlab-mode-hook
                      sh-mode-hook
                      web-mode-hook
                      html-mode-hook
                      css-mode-hook
                      makefile-gmake-mode-hook))
        (remove-hook hook 'nlinum-mode)))

    (turn-on-nlinum)))


(setq setup-linum-loaded t) ;; required.. used in setup-visual
(provide 'setup-linum)
