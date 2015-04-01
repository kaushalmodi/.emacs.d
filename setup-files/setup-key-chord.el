;; Time-stamp: <2015-04-01 08:59:07 kmodi>

;; Key Chord Mode
;; http://www.emacswiki.org/emacs/key-chord.el

(use-package key-chord
  :config
  (progn
    (key-chord-mode 1)))


(provide 'setup-key-chord)

;; John Cook's post http://www.johndcook.com/blog/2015/02/01/rare-bigrams/
;; provides a list of rare bi-grams that would work great for key-chords
;;      bf bg bq bx bz
;;      cj cv
;;      dx dz
;;      fb fj fq fv fz
;;      gb gj gp gq gv gx gz
;;      hj hq hv
;;  jj  jc jf jg jh jk jl jm jp jq js jt jv jw jx jy jz
;;  kk  kj kq kv kx kz
;;      lj lq
;;      mj mq mv mx mz
;;      pg pj pq pv pz
;;  qq  qb qf qg qh qj qk ql qm qp qt qv qw qx qy qz
;;      sj sx sz
;;      tj tq
;;  vv  vc vf vg vh vj vk vm vp vq vw vz
;;  ww  wj wq wv wx wz
;;      xb xd xg xj xk xm xq xs xw xz
;;  yy  yj yq
;;      zb zd zf zg zj zk zm zp zq zs zv zw zx
