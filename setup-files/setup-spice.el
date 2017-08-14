;; Time-stamp: <2017-08-14 13:36:14 kmodi>

;; Hspice, Spice

(use-package spice-mode
  :load-path "elisp/spice-mode"
  :mode (("\\.sp\\'"  . spice-mode)
         ("\\.cir\\'" . spice-mode)
         ("\\.ckt\\'" . spice-mode)
         ("\\.sckt\\'" . spice-mode)
         ("\\.mod\\'" . spice-mode)
         ("\\.cdl\\'" . spice-mode)
         ("\\.chi\\'" . spice-mode)
         ("\\.inp\\'" . spice-mode)))


(provide 'setup-spice)
