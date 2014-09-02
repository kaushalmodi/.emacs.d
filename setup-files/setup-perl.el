;; Time-stamp: <2014-08-13 11:07:29 kmodi>

;; Perl

(req-package cperl-mode
;; Use cperl-mode instead of the default perl-mode
  :mode ("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode)
  :interpreter (("perl"     . cperl-mode)
                ("perl5"    . cperl-mode)
                ("miniperl" . cperl-mode))
  :config
  (progn
    (setq cperl-indent-level 3
          cperl-close-paren-offset -3
          cperl-continued-statement-offset 3
          cperl-indent-parens-as-block t
          cperl-tab-always-indent t)

    (defun my-cperl-mode-customizations()
      (req-package setup-editing)
      (define-key cperl-mode-map "\177"      'delete-backward-char)
      (define-key cperl-mode-map (kbd "C-j") 'pull-up-line))

    (add-hook 'cperl-mode-hook 'my-cperl-mode-customizations)))


(provide 'setup-perl)
