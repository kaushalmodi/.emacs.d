;; Time-stamp: <2016-11-05 10:54:26 kmodi>

;; Web Mode
;; http://web-mode.org

(use-package web-mode
  :mode ("\\.html?\\'"
         "\\.php\\'"
         "\\.as[cp]x\\'")
  :config
  (progn
    (setq web-mode-engines-alist '(("gtl" . ".*hugo.*html\\'"))))) ; Go Template


(provide 'setup-web-mode)
