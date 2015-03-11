;; Time-stamp: <2015-03-11 09:47:35 kmodi>

;; which-func
;; which-function

(use-package which-func
    :config
  (progn
    ;; Don't set `which-function-mode' to be enabled by default for all modes
    ;; Major modes needing this mode should do:
    ;;   (add-to-list 'which-func-mode 'MAJOR-MODE)
    (setq which-func-modes nil)))


(provide 'setup-which-func)
