;; Time-stamp: <2015-01-27 12:10:50 kmodi>

;; Sunshine - weather forecast
;; Source: https://github.com/aaronbieber/sunshine.el

(req-package sunshine
  :load-path "from-git/sunshine.el"
  :config
  (progn
    (setq sunshine-location "27278,USA")
    (setq sunshine-show-icons t)

    (bind-to-modi-map "w" sunshine-forecast)))


(provide 'setup-sunshine)
