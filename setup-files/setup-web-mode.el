;; Time-stamp: <2014-08-13 09:53:41 kmodi>

;; Web Mode
;; Source: http://web-mode.org/

(req-package web-mode
  :mode (("\\.html?\\'"     . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.as[cp]x\\'"   . web-mode)))


(provide 'setup-web-mode)
