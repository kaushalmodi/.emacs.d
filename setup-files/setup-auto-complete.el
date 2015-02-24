;; Time-stamp: <2015-02-24 13:05:37 kmodi>

;; Auto complete

(use-package auto-complete-config
  :idle
  (progn
    (setq ac-stop-words (quote ("/" "//" "/*" "//*" "///" "////")))
    (setq ac-ignore-case t) ; ignore case
    (setq ac-use-fuzzy t) ; enable fuzzy auto complete
    (setq ac-trigger-key "TAB")

    ;; http://cx4a.org/software/auto-complete/manual.html#Select_candidates_with_C-n_C-p_only_when_completion_menu_is_displayed
    ;; Use C-n/p instead of arrow keys to select ac options from the ac menu
    (setq ac-use-menu-map t)
    (bind-keys
     :map ac-menu-map
     ("C-n" . ac-next)
     ("C-p" . ac-previous))
    (ac-config-default)))


(provide 'setup-auto-complete)
