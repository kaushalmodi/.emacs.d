;; Time-stamp: <2014-10-30 19:03:21 kmodi>

;; Linum Relative
(req-package linum-relative
  :require (linum)
  :config
  (progn
    (defun modi/turn-on-linum ()
      "Turn on linum mode in specific modes."
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
        (add-hook hook 'linum-mode)))

    (defun modi/turn-off-linum ()
      "Unhook linum mode from various major modes."
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
        (remove-hook hook 'linum-mode)))

    (modi/turn-on-linum)
    (setq linum-relative-current-symbol "")
    ;; The symbol you want to show on the current line, by default it is 0.
    ;; You can use any string like \"->\". If this variable is empty string,
    ;; linum-releative will show the real line number at current line.
    ))

;; ;; NLinum
;; (req-package nlinum
;;   :require (linum verilog-mode cperl-mode python-mode)
;;   :config
;;   (progn
;;     (global-linum-mode -1) ; Disable linum-mode globally
;;     (global-nlinum-mode -1) ; Disable nlinum-mode globally
;;     (setq nlinum-format "%4d ") ; right aligned, 4 char wide line num col

;;     (defun modi/turn-on-nlinum ()
;;       "Turn on nlinum mode in specific modes."
;;       (interactive)
;;       (dolist (hook '(verilog-mode-hook
;;                       emacs-lisp-mode-hook
;;                       cperl-mode-hook
;;                       c-mode-hook
;;                       python-mode-hook
;;                       matlab-mode-hook
;;                       sh-mode-hook
;;                       web-mode-hook
;;                       html-mode-hook
;;                       css-mode-hook
;;                       makefile-gmake-mode-hook))
;;         (add-hook hook 'nlinum-mode)))

;;     (defun modi/turn-off-nlinum ()
;;       "Unhook nlinum mode from various major modes."
;;       (interactive)
;;       (dolist (hook '(verilog-mode-hook
;;                       emacs-lisp-mode-hook
;;                       cperl-mode-hook
;;                       c-mode-hook
;;                       python-mode-hook
;;                       matlab-mode-hook
;;                       sh-mode-hook
;;                       web-mode-hook
;;                       html-mode-hook
;;                       css-mode-hook
;;                       makefile-gmake-mode-hook))
;;         (remove-hook hook 'nlinum-mode)))

;;     (modi/turn-on-nlinum)))


(provide 'setup-linum)
