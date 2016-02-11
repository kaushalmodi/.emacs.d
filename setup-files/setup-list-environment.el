;; Time-stamp: <2016-02-11 16:30:57 kmodi>

;; List Environment
;; https://github.com/dgtized/list-environment.el

(use-package list-environment
  :config
  (progn
    (with-eval-after-load 'stripe-buffer
      (defun modi/list-environment (orig-fun &rest args)
        (let ((truncate-partial-width-windows nil))
          (prog1 ; Return value of the advising fn needs to be the same as ORIG-FUN
              (apply orig-fun args)
            (setq-local line-move-visual nil) ; logical line navigation
            (set-window-fringes nil 0 0) ; disable fringes in current window
            (toggle-truncate-lines -1)
            (stripe-listify-buffer))))
      (advice-add 'list-environment :around #'modi/list-environment))))


(provide 'setup-list-environment)
