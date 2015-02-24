;; Time-stamp: <2015-02-24 11:51:14 kmodi>

;; Hspice, Spice

(use-package spice-mode
  :load-path "elisp/spice-mode"
  :mode (("\\.sp\\'"  . spice-mode)
         ("\\.cir\\'" . spice-mode)
         ("\\.ckt\\'" . spice-mode)
         ("\\.mod\\'" . spice-mode)
         ("\\.cdl\\'" . spice-mode)
         ("\\.chi\\'" . spice-mode)
         ("\\.inp\\'" . spice-mode)))


(provide 'setup-spice)
