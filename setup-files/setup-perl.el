;; Time-stamp: <2014-03-21 14:44:15 kmodi>

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


(setq setup-perl-loaded t)
(provide 'setup-perl)
