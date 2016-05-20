;; Time-stamp: <2016-05-19 22:15:43 kmodi>

;; Weather Forecast

;; Sunshine
;; https://github.com/aaronbieber/sunshine.el
(use-package sunshine
  :defer t
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
  :defer t
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

(defun modi/weather (arg)
  "Display the weather in varying detail as specified by ARG.

If ARG is nil,  call `sunshine-quick-forecast'.
If ARG is (4),  call `sunshine-forecast'.
If ARG is (16), call `forecast'."
  (interactive "p")
  (cl-case arg
    (4  (call-interactively #'sunshine-forecast))
    (16 (call-interactively #'forecast))
    (t  (call-interactively #'sunshine-quick-forecast))))


(provide 'setup-weather)
