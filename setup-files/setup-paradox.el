;; Time-stamp: <2015-10-14 16:24:24 kmodi>

;; Paradox
;; https://github.com/Malabarba/paradox

(use-package paradox
  :commands (package-list-packages
             paradox-list-packages
             hydra-launch/paradox-list-packages-and-exit)
  :config
  (progn
    ;; The "paradox-token" file is supposed to contain this line:
    ;;     (setq paradox-github-token "<YOUR_TOKEN>")
    (load (locate-user-emacs-file "paradox-token") :noerror :nomessage)

    (setq paradox-lines-per-entry 1)
    (setq paradox-automatically-star t)

    (define-key paradox--filter-map "k" nil) ; Unbind redundant key

    (defhydra hydra-paradox-filter (:color blue
                                    :hint nil)
      "
_f_ilter                 filter _r_egexp            _i_nstalled      _d_ependencies
g_n_u elpa packages      available _u_pgrades       _a_vailable      _c_lear filter
_m_elpa packages         packages with _s_tars      _b_uilt-in
"
      ("f" package-menu-filter)
      ("n" (package-menu-filter "arc:gnu"))
      ("m" (package-menu-filter "arc:melpa"))
      ("r" paradox-filter-regexp)
      ("u" paradox-filter-upgrades)
      ("s" paradox-filter-stars)
      ("i" (package-menu-filter "status:installed"))
      ("a" (package-menu-filter "status:available"))
      ("b" (package-menu-filter "status:built-in"))
      ("d" (package-menu-filter "status:dependency"))
      ("c" paradox-filter-clear)
      ("g" paradox-filter-clear)
      ("q" nil "cancel" :color blue))
    (bind-key "f" #'hydra-paradox-filter/body paradox-menu-mode-map)

    (paradox-enable)))


(provide 'setup-paradox)
