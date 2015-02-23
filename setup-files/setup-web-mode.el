;; Time-stamp: <2015-02-23 11:42:32 kmodi>

;; Web Mode
;; Source: http://web-mode.org/

(use-package web-mode
  :mode (("\\.html?\\'"     . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.as[cp]x\\'"   . web-mode)))


(provide 'setup-web-mode)
