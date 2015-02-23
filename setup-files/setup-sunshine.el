;; Time-stamp: <2015-02-23 11:42:33 kmodi>

;; Sunshine - weather forecast
;; Source: https://github.com/aaronbieber/sunshine.el

(use-package sunshine
  :config
  (progn
    (setq sunshine-location "27278,USA")
    (setq sunshine-show-icons t)

    (bind-to-modi-map "w" sunshine-quick-forecast)
    (bind-to-modi-map "W" sunshine-forecast)))


(provide 'setup-sunshine)
