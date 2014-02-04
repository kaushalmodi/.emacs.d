;; Time-stamp: <2013-12-12 15:46:22 kmodi>

;; Auto complete

(require 'auto-complete-config)

(setq ac-stop-words (quote ("/" "//" "/*" "//*" "///" "////"))
      ac-ignore-case t ;; ignore case
      ac-use-fuzzy t ;; enable fuzzy auto complete
      ac-trigger-key "TAB"
      )

;; Source: http://cx4a.org/software/auto-complete/manual.html#Select_candidates_with_C-n_C-p_only_when_completion_menu_is_displayed
;; Use C-n/p instead of arrow keys to select ac options from the ac menu
(setq ac-use-menu-map t)
;; Default settings
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

(ac-config-default)

(setq setup-auto-complete-loaded t)
(provide 'setup-auto-complete)
