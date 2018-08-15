;; Time-stamp: <2018-08-14 21:53:12 kmodi>
;; Gujarati - ગુજરાતી

(setq default-input-method "gujarati-itrans") ;Gujarati transliteration

;; Below will cause to toggle between `default-input-method' and
;; "normal" i.e. no input method.
(bind-to-modi-map "\\" #'toggle-input-method)


(provide 'setup-gujarati)
