;; Time-stamp: <2017-06-21 11:32:23 kmodi>

;; Ibuffer

(use-package ibuffer
  :bind (:map modi-mode-map
         ([remap list-buffers] . ibuffer))
  :config
  (progn
    (setq ibuffer-default-sorting-mode 'major-mode)

    ;; Do not prompt when executing 'dangerous' operations; the ones tagged with
    ;; ":dangerous t" when defined using the `define-ibuffer-op' macro.
    ;; Fri Jun 16 12:04:05 EDT 2017 - kmodi
    ;; As of today only buffer killing operations are marked as dangerous.
    ;; Actually it's not so dangerous, as kill confirmation  prompts still
    ;; show up for modified buffers regardless of the value of `ibuffer-expert'.
    ;; So setting `ibuffer-expert' to t will enable promptless-killing of only
    ;; unmodified buffers.. which is OK.
    (setq ibuffer-expert t)

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

(use-package ibuf-ext
  :defer t
  :config
  (progn
    ;; Do not show empty groups
    (setq ibuffer-show-empty-filter-groups nil)))


(provide 'setup-ibuffer)
