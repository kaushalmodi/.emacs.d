;; Time-stamp: <2018-04-04 10:45:03 kmodi>

;; Web Mode
;; http://web-mode.org

(use-package web-mode
  :mode ("\\.html?\\'"
         "\\.php\\'"
         "\\.xml\\'"
         "\\.as[cp]x\\'")
  :load-path "elisp/web-mode"
  :config
  (progn
    (setq web-mode-engines-alist '(("hugo" . ".*hugo.*\\(html\\|xml\\)\\'"))))) ;Go Template


(provide 'setup-web-mode)
