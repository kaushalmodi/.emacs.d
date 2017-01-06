;; Time-stamp: <2017-01-06 14:46:14 kmodi>

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

    (setq forecast-api-url "https://api.darksky.net")

    ;; The "darksky-api" file is supposed to contain this line:
    ;;     (setq forecast-api-key "<YOUR_API>")
    ;; Register at https://darksky.net/dev/account/ to get your API KEY.
    (load (locate-user-emacs-file "darksky-api") :noerror :nomessage)

    ;; The below calendar-* variables from `solar.el' are used by `forecast.el'.
    (use-package solar
      :config
      (progn
        (setq calendar-latitude 36.070556)
        (setq calendar-longitude -79.104167)
        (setq calendar-location-name "Hillsborough, NC")))

    (setq forecast-units 'us)

    ;; Patch the forecast--insert-io-link to change references to Dark Sky
    (defun forecast--insert-io-link ()
      "Insert link to Dark Sky."
      (newline)
      (insert "Powered by")
      (insert " ")
      (insert-text-button
       "Dark Sky"
       'follow-link t 'action
       (lambda (b)
         (ignore b)
         (browse-url (format "https://darksky.net/forecast/%s,%s/us12/en"
                             (number-to-string calendar-latitude)
                             (number-to-string calendar-longitude))))))))

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
