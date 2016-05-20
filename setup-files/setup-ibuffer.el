;; Time-stamp: <2016-05-19 22:03:47 kmodi>

;; Ibuffer

(use-package ibuffer
  :bind (:map modi-mode-map
         ("C-x C-b" . ibuffer)) ; Override default binding for `buffer-menu'
  :config
  (progn
    (setq ibuffer-default-sorting-mode 'major-mode)

    (use-package ibuffer-projectile
      :config
      (progn
        (defun my/ibuffer-customization ()
          ;; ibuffer-projectile setup
          (ibuffer-projectile-set-filter-groups)
          (unless (eq ibuffer-sorting-mode 'alphabetic)
            (ibuffer-do-sort-by-alphabetic) ; first do alphabetic sort
            (ibuffer-do-sort-by-major-mode))))) ; then do major-mode sort

    ;; ibuffer-projectile setup
    (add-hook 'ibuffer-hook #'my/ibuffer-customization)))


(provide 'setup-ibuffer)
