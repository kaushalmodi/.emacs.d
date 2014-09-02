;; Time-stamp: <2014-08-13 12:01:22 kmodi>

;; Auto complete

(req-package auto-complete-config
  :config
  (progn
    (setq ac-stop-words (quote ("/" "//" "/*" "//*" "///" "////"))
          ac-ignore-case t ;; ignore case
          ac-use-fuzzy t ;; enable fuzzy auto complete
          ac-trigger-key "TAB"
          ;; Source: http://cx4a.org/software/auto-complete/manual.html#Select_candidates_with_C-n_C-p_only_when_completion_menu_is_displayed
          ;; Use C-n/p instead of arrow keys to select ac options from the ac menu
          ac-use-menu-map t)
    (bind-keys
     :map ac-menu-map
     ("C-n" . ac-next)
     ("C-p" . ac-previous))
    (ac-config-default)))


(provide 'setup-auto-complete)
