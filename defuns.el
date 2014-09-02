;; Time-stamp: <2014-08-13 15:29:53 kmodi>

;; Collection of general purposes defuns and macros

;; Save typing the lambda mumbo-jumbo
;; Source: https://github.com/waymondo/hemacs/blob/master/defuns.el
(defmacro λ (&rest body)
  (declare (indent 1))
  `(lambda ()
     (interactive)
     ,@body))
(key-chord-define-global "^^" (λ (insert "λ")))

(provide 'defuns)
