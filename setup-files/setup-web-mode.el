;; Time-stamp: <2014-02-05 02:27:25 Kaushal>

;; Web Mode
;; Source: http://web-mode.org/

(require 'web-mode)

(setq auto-mode-alist
      (append
       '(
         ("\\.html?\\'"     . web-mode)
         ("\\.phtml\\'"     . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.jsp\\'"       . web-mode)
         ("\\.as[cp]x\\'"   . web-mode)
         ("\\.erb\\'"       . web-mode)
         ("\\.mustache\\'"  . web-mode)
         ("\\.djhtml\\'"    . web-mode)
         ) auto-mode-alist))


(setq setup-web-mode-loaded t)
(provide 'setup-web-mode)
