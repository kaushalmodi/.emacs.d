;; Time-stamp: <2015-01-23 10:20:50 kmodi>

;; Ibuffer

(req-package ibuffer
  :require (projectile ibuffer-projectile)
  :config
  (progn
    (setq ibuffer-default-sorting-mode 'major-mode)

    (defun my/ibuffer-customization ()
      ;; ibuffer-projectile setup
      (ibuffer-projectile-set-filter-groups)
      (unless (eq ibuffer-sorting-mode 'alphabetic)
        (ibuffer-do-sort-by-alphabetic) ; first do alphabetic sort
        (ibuffer-do-sort-by-major-mode) ; then do major-mode sort
        ))

    ;; ibuffer-projectile setup
    (add-hook 'ibuffer-hook #'my/ibuffer-customization)

    (bind-keys
     :map modi-mode-map
     ("C-x C-b" . ibuffer)))) ; replace buffer-menu with ibuffer


(provide 'setup-ibuffer)
