;; Time-stamp: <2014-03-25 12:12:48 kmodi>

;; Perl

;; Use cperl-mode instead of the default perl-mode
(add-to-list 'auto-mode-alist        '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl"                       . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5"                      . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl"                   . cperl-mode))

(setq cperl-indent-level 4
      cperl-close-paren-offset -4
      cperl-continued-statement-offset 4
      cperl-indent-parens-as-block t
      cperl-tab-always-indent t)

(defun my-cperl-mode-customizations()
  (when (boundp 'setup-editing-loaded)
    (define-key cperl-mode-map (kbd "C-j") 'pull-up-line))
  )
(add-hook 'cperl-mode-hook 'my-cperl-mode-customizations)


(setq setup-perl-loaded t)
(provide 'setup-perl)
