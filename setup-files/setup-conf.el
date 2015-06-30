;; Time-stamp: <2015-06-30 16:51:07 kmodi>

;; Conf Mode

(use-package conf-mode
  :mode (("\\.conf\\'"    . conf-space-mode)
         ("\\.setup.*\\'" . conf-space-mode)))


(provide 'setup-conf)
