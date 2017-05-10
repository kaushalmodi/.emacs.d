;; Time-stamp: <2017-05-10 09:26:23 kmodi>

;; Ibuffer

(use-package ibuffer
  :bind (:map modi-mode-map
         ([remap list-buffers] . ibuffer))
  :config
  (progn
    (setq ibuffer-default-sorting-mode 'major-mode)

    (use-package ibuffer-projectile
      :config
      (progn
        (defun modi/ibuffer-customization ()
          "My customization for `ibuffer'."
          ;; ibuffer-projectile setup
          (ibuffer-projectile-set-filter-groups)
          (unless (eq ibuffer-sorting-mode 'alphabetic)
            (ibuffer-do-sort-by-alphabetic) ; first do alphabetic sort
            (ibuffer-do-sort-by-major-mode))))) ; then do major-mode sort

    ;; ibuffer-projectile setup
    (add-hook 'ibuffer-hook #'modi/ibuffer-customization)))


(provide 'setup-ibuffer)
