;; Time-stamp: <2015-10-13 11:17:18 kmodi>

;; Info

(use-package info
  :commands (info)
  :init
  (progn
    (defhydra hydra-info (:color blue
                          :hint nil)
      "
Info-mode:

  ^^_]_ forward  (next logical node)       ^^_l_ast (←)                     _u_p (↑)                             _f_ollow reference       _d_irectory of all manuals
  ^^_[_ backward (prev logical node)       ^^_r_eturn (→)                   _m_enu (↓) (C-u for new window)      _i_ndex                  _T_OC of current manual
  ^^_n_ext (same level only)               ^^_H_istory                      _g_oto (C-u for new window)          _,_ next index item      _w_ copy node name
  ^^_p_rev (same level only)               _<_/_t_op of current manual      _b_eginning of buffer                virtual _I_ndex          _c_lone buffer
  regex _s_earch (_S_ case sensitive)      ^^_>_ final                      _e_nd of buffer                      ^^                       _a_propos

  _<backspace>_/_<SPC>_ Scroll up/down     _1_ .. _9_ Pick first .. ninth item in the node's menu.

"
      ("]"   Info-forward-node)
      ("["   Info-backward-node)
      ("n"   Info-next)
      ("p"   Info-prev)
      ("s"   Info-search)
      ("S"   Info-search-case-sensitively)

      ("l"   Info-history-back)
      ("r"   Info-history-forward)
      ("H"   Info-history)
      ("t"   Info-top-node)
      ("<"   Info-top-node)
      (">"   Info-final-node)

      ("u"   Info-up)
      ("^"   Info-up)
      ("m"   Info-menu)
      ("g"   Info-goto-node)
      ("b"   beginning-of-buffer)
      ("e"   end-of-buffer)

      ("f"   Info-follow-reference)
      ("i"   Info-index)
      (","   Info-index-next)
      ("I"   Info-virtual-index)

      ("d"   Info-directory)
      ("T"   Info-toc)
      ("w"   Info-copy-current-node-name) ; M-0 w will copy elisp form of current node name
      ("c"   clone-buffer)
      ("a"   info-apropos)

      ("1"   Info-nth-menu-item)
      ("2"   Info-nth-menu-item)
      ("3"   Info-nth-menu-item)
      ("4"   Info-nth-menu-item)
      ("5"   Info-nth-menu-item)
      ("6"   Info-nth-menu-item)
      ("7"   Info-nth-menu-item)
      ("8"   Info-nth-menu-item)
      ("9"   Info-nth-menu-item)

      ("<backspace>" Info-scroll-down)
      ("<SPC>" Info-scroll-up)

      ("?"   Info-summary "Info summary")
      ("h"   Info-help "Info help")
      ("q"   Info-exit "Info exit")
      ("C-g" nil "cancel" :color blue))
    (bind-key "?" #'hydra-info/body Info-mode-map))
  :config
  (progn
    (>=e "25.0"
        (when (find-font (font-spec :name "DejaVu Sans Mono"))
          (set-face-attribute 'Info-quoted nil :family "DejaVu Sans Mono")))

    (use-package info+
      :config
      (progn
        ;; Override the Info-mode-map binding to "?" set by info+
        (bind-key "?" #'hydra-info/body Info-mode-map)
        ;; Allow mouse scrolling to do its normal thing
        (define-key Info-mode-map [mouse-4] nil)
        (define-key Info-mode-map [mouse-5] nil)))))


(provide 'setup-info)
