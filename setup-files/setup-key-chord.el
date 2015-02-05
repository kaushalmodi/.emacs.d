;; Time-stamp: <2015-02-04 10:45:34 kmodi>

;; Key Chord Mode
;; http://www.emacswiki.org/emacs/key-chord.el
(req-package key-chord
  :config
  (progn
    (key-chord-mode 1)))


(provide 'setup-key-chord)

;; John Cook's post http://www.johndcook.com/blog/2015/02/01/rare-bigrams/
;; provides a list of rare bi-grams that would work great for key-chords
;;      BF BG BQ BX BZ
;;      CJ CV
;;      DX DZ
;;      FB FJ FQ FV FZ
;;      GB GJ GP GQ GV GX GZ
;;      HJ HQ HV
;;  JJ  JC JF JG JH JK JL JM JP JQ JS JT JV JW JX JY JZ
;;  KK  KJ KQ KV KX KZ
;;      LJ LQ
;;      MJ MQ MV MX MZ
;;      PG PJ PQ PV PZ
;;  QQ  QB QF QG QH QJ QK QL QM QP QT QV QW QX QY QZ
;;      SJ SX SZ
;;      TJ TQ
;;  VV  VC VF VG VH VJ VK VM VP VQ VW VZ
;;  WW  WJ WQ WV WX WZ
;;      XB XD XG XJ XK XM XQ XS XW XZ
;;  YY  YJ YQ
;;      ZB ZD ZF ZG ZJ ZK ZM ZP ZQ ZS ZV ZW ZX
