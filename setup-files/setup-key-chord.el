;; Time-stamp: <2015-04-15 13:00:28 kmodi>

;; Key Chord Mode
;; http://www.emacswiki.org/emacs/key-chord.el

(use-package key-chord
  :config
  (progn
    (setq key-chord-two-keys-delay 0.05)
    (key-chord-mode 1)))


(provide 'setup-key-chord)

;; John Cook's post http://www.johndcook.com/blog/2015/02/01/rare-bigrams/
;; provides a list of rare bi-grams that would work great for key-chords.

;; Below list is based off that after removing all the key-chord duplicates
;; like `xs' and `sx'.

;;      fb
;;      gb gp
;;  jj  jc jf jg jh jk jl jm jp jq js jt jv jw jx jy jz
;;  kk
;;  qq  qb qf qg qh qk ql qm qp qt qv qw qx qy qz
;;  vv  vc vf vg vh vk vm vp vw vz
;;  ww
;;      xb xd xg xk xm xs xw
;;  yy
;;      zb zd zf zg zk zm zp zs zw zx
