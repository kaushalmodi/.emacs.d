;; Time-stamp: <2019-03-23 23:25:29 kmodi>

;; Hspice, Spice

(use-package spice-mode
  :load-path "elisp/spice-mode"
  :mode (("\\.sp\\'"  . spice-mode)
         ("\\.cir\\'" . spice-mode)
         ("\\.ckt\\'" . spice-mode)
         ("\\.sckt\\'" . spice-mode)
         ("\\.cdl\\'" . spice-mode)
         ("\\.chi\\'" . spice-mode)
         ("\\.inp\\'" . spice-mode)
         ("[^g][^o]\\.mod\\'" . spice-mode)))  ;*<XY>.mod unless <XY>=="go"


(provide 'setup-spice)
