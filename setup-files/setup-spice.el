;; Time-stamp: <2019-07-31 08:03:09 kmodi>

;; Hspice, Spice

(use-package spice-mode
  :load-path "elisp/manually-synced/spice-mode"
  :mode (("\\.sp\\'"  . spice-mode)
         ("\\.cir\\'" . spice-mode)
         ("\\.ckt\\'" . spice-mode)
         ("\\.sckt\\'" . spice-mode)
         ("\\.cdl\\'" . spice-mode)
         ("\\.chi\\'" . spice-mode)
         ("\\.inp\\'" . spice-mode)
         ("[^g][^o]\\.mod\\'" . spice-mode)))  ;*<XY>.mod unless <XY>=="go"


(provide 'setup-spice)
