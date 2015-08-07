;; Time-stamp: <2015-08-07 15:40:06 kmodi>

;; Temporary minor mode

;; http://emacs.stackexchange.com/a/524/115
;; Main use is to enable it using File Local Variables to get buffer-specific
;; key bindings.
;;   For example, put something like below in the “Local Variables:” block:
;; eval: (temp-mode 1)
;; eval: (define-key temp-mode-map (kbd "<f10>") #'some-special-fn)

(defvar temp-mode-map (make-sparse-keymap)
  "Keymap while temp-mode is active.")

;;;###autoload
(define-minor-mode temp-mode
  "A temporary minor mode used to set buffer-specific key bindings."
  nil
  :lighter " Temp"
  temp-mode-map)


(provide 'temp-mode)
