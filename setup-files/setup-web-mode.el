;; Time-stamp: <2018-02-26 16:18:30 kmodi>

;; Web Mode
;; http://web-mode.org

(use-package web-mode
  :mode ("\\.html?\\'"
         "\\.php\\'"
         "\\.as[cp]x\\'")
  :load-path "elisp/web-mode"
  :config
  (progn
    (setq web-mode-engines-alist '(("hugo" . ".*hugo.*html\\'"))))) ;Go Template


(provide 'setup-web-mode)
