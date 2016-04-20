;; Time-stamp: <2016-04-20 14:40:22 kmodi>

;; Toggles

(defhydra hydra-toggle (:color blue
                        :hint  nil)
  "
    TOGGLE ...
_ai_ aggressive indent      cycle _c_ase^^                    _F_ollow            _H_ardcore (allow arrows)      _m_odi mode          _t_ truncate lines
_ar_ auto revert            _d_/_D_ debug on error/entry      indent _g_uide      _i_menu list                   _p_resentation       _C-t_ theme
_aw_ adaptive wrap          _e_debug^^                        _hl_ hl-line        _k_ey chord                    _r_ead only          _v_isible mode
menu _b_ar                  _f_ill^^                          _hs_ hideshow       _l_ine num                     _sa_ scroll all      _<SPC>_ whitespace
"
  ("ai"    aggressive-indent-mode)
  ("ar"    auto-revert-mode)
  ("aw"    adaptive-wrap-prefix-mode)
  ("b"     modi/toggle-menu-bar)
  ("c"     xah-cycle-letter-case :color red)
  ("d"     toggle-debug-on-error)
  ("D"     modi/toggle-debug)
  ("e"     modi/toggle-edebug)
  ("f"     toggle-fill-unfill)
  ("F"     follow-mode)
  ("g"     indent-guide-mode)
  ("hl"    hl-line-mode)
  ("hs"    modi/hideshow-mode)
  ("H"     hardcore-mode)
  ("i"     modi/imenu-list-display-toggle)
  ("k"     key-chord-mode :color red)
  ("l"     modi/toggle-linum)
  ("m"     modi-mode)
  ("n"     endless/narrow-or-widen-dwim)
  ("N"     neotree-toggle)
  ("o"     modi/toggle-one-window :color red)
  ("p"     prez-mode)
  ("r"     dired-toggle-read-only) ; generalized `read-only-mode'
  ("sa"    scroll-all-mode)
  ("t"     toggle-truncate-lines :color red)
  ("C-t"   toggle-theme)
  ("v"     visible-mode)
  ("<SPC>" whitespace-mode :color red)
  ("q"     nil "cancel" :color blue))

(bind-key "s-t" #'hydra-toggle/body)
(bind-key "C-c t" #'hydra-toggle/body)
(key-chord-define-global "hj" #'hydra-toggle/body)


(provide 'setup-toggles)

;; http://endlessparentheses.com/the-toggle-map-and-wizardry.html
