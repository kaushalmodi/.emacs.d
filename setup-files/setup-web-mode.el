;; Time-stamp: <2014-02-04 15:13:51 kmodi>

;; Web Mode
;; Source: http://web-mode.org/

(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)) ;; plain html

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))


(setq setup-web-mode-loaded t)
(provide 'setup-web-mode)
