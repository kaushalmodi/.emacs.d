;; Time-stamp: <2015-01-13 19:55:03 kmodi>

;; Emacs Lisp Mode

(defun my/edebug-defun ()
  (interactive)
  (eval-defun :edebug))

;; edebug
(bind-keys
 :map modi-mode-map
 ("<f7>" . my/edebug-defun) ; add edebug instrumentation
 ("<S-f7>" . eval-defun)) ; remove edebug instrumentation


(provide 'setup-elisp)
