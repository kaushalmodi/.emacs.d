;; Time-stamp: <2015-10-21 10:08:10 kmodi>

;; Weather Forecast

;; Sunshine
;; https://github.com/aaronbieber/sunshine.el
(use-package sunshine
  :commands (sunshine-quick-forecast
             sunshine-forecast
             hydra-launch/sunshine-quick-forecast-and-exit
             hydra-launch/sunshine-forecast-and-exit)
  :config
  (progn
    ;; The "openweathermap-api" file is supposed to contain this line:
    ;;     (setq sunshine-appid "<YOUR_API>")
    ;; Sign up at http://openweathermap.org/ to get your API KEY.
    (load (locate-user-emacs-file "openweathermap-api") :noerror :nomessage)

    (setq sunshine-location "27278,USA")
    (setq sunshine-show-icons t)))

;; Forecast
;; https://github.com/cadadr/forecast.el
(use-package forecast
  ;; deferring not needed as the package is set to autoload on M-x forecast
  ;; :defer 1 ; Wait for at least a second after emacs has loaded.
  ;;          ; The emacs frame needs to be set up properly before `find-font' call.
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
