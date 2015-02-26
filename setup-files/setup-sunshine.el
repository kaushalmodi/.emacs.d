;; Time-stamp: <2015-02-26 12:36:39 kmodi>

;; Sunshine - weather forecast
;; Source: https://github.com/aaronbieber/sunshine.el

(use-package sunshine
    :commands (sunshine-quick-forecast sunshine-forecast)
    :init
    (progn
      (bind-to-modi-map "w" sunshine-quick-forecast)
      (bind-to-modi-map "W" sunshine-forecast))
    :config
    (progn
      (setq sunshine-location "27278,USA")
      (setq sunshine-show-icons t)))


(provide 'setup-sunshine)
