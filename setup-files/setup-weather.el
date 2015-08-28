;; Time-stamp: <2015-08-28 13:23:45 kmodi>

;; Weather Forecast

;; Sunshine
;; https://github.com/aaronbieber/sunshine.el

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

;; Forecast
;; https://github.com/cadadr/forecast.el

(use-package forecast
  :defer 1 ; Wait for at least a second after emacs has loaded.
                                        ; The emacs frame needs to be set up
                                        ; properly before the `find-font' call.
  :load-path "elisp/forecast.el"
  :commands (forecast)
  :config
  (progn
    ;; Use Quivira font for moon phases
    (when (find-font (font-spec :name "Quivira"))
      (set-face-attribute 'forecast-moon-phase nil :font "Quivira"))

    (set-face-attribute 'forecast-upcoming-temperature nil
                        :inherit font-lock-function-name-face)

    ;; The "forecast-api" file is supposed to contain this line:
    ;;     (setq forecast-api-key "<YOUR_API>")
    ;; Register at https://developer.forecast.io/ to get your API KEY.
    (load (locate-user-emacs-file "forecast-api") :noerror :nomessage)

    (setq forecast-latitude  36.070556)
    (setq forecast-longitude -79.104167)
    (setq forecast-city      "Hillsborough, NC")
    (setq forecast-country   "USA")
    (setq forecast-units     'us)))


(provide 'setup-weather)
