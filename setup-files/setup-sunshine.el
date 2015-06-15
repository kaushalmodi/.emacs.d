;; Time-stamp: <2015-06-15 17:08:48 kmodi>

;; Sunshine - weather forecast
;; Source: https://github.com/aaronbieber/sunshine.el

(use-package sunshine
    :commands (sunshine-quick-forecast sunshine-forecast)
    :init
    (progn
      (bind-to-modi-map "w" #'sunshine-quick-forecast)
      (bind-to-modi-map "W" #'sunshine-forecast))
    :config
    (progn
      (setq sunshine-location "27278,USA")
      (setq sunshine-show-icons t)))


(provide 'setup-sunshine)
