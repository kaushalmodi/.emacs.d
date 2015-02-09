;; Time-stamp: <2015-02-09 09:59:42 kmodi>

;; Number
;; https://github.com/chrisdone/number

(req-package number
  :config
  (progn
    (defhydra hydra-math
        (nil "C-c m" :bind (lambda (key cmd) (bind-key key cmd modi-mode-map)))
      "math-operation"
      ("+" number/add      "Add")
      ("-" number/sub      "Sub")
      ("*" number/multiply "Mul")
      ("/" number/divide   "Div")
      ("0" number/pad      "Pad 0s")
      ("=" number/eval     "Eval"))))


(provide 'setup-number)
