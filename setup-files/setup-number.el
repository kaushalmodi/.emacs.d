;; Time-stamp: <2015-01-23 14:29:31 kmodi>

;; Number
;; https://github.com/chrisdone/number

(req-package number
  :config
  (progn
    (hydra-create "C-c m"
      '(("+" number/add      "Add")
        ("-" number/sub      "Sub")
        ("*" number/multiply "Mul")
        ("/" number/divide   "Div")
        ("0" number/pad      "Pad 0s")
        ("=" number/eval     "Eval"))
      modi-mode-map)))


(provide 'setup-number)
