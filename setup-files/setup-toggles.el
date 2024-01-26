;; Time-stamp: <2024-01-26 18:45:13 kmodi>

;; Toggles

(defhydra hydra-toggle (:color blue
                        :columns 7)
  "Toggle"
  ("aa"    artist-mode "artist mode")
  ("af"    auto-fill-mode "auto fill")
  ("ar"    auto-revert-mode "auto revert")
  ("aw"    adaptive-wrap-prefix-mode "adaptive wrap")
  ("b"     modi/toggle-menu-bar "menu bar")
  ("c"     xah-cycle-letter-case "cycle case" :color red)
  ("d"     toggle-debug-on-error "debug on error")
  ("D"     modi/toggle-debug "debug on entry")
  ("e"     modi/toggle-edebug "edebug")
  ("f"     toggle-fill-unfill "fill/unfill")
  ("F"     follow-mode "follow mode")
  ("g"     indent-guide-mode "indent guide")
  ("hc"    hardcore-mode "arrow key navigation")
  ("hl"    hl-line-mode "highlight line")
  ("hs"    modi/hideshow-mode "hideshow mode")
  ("i"     modi/imenu-list-display-toggle "imenu list")
  ("k"     key-chord-mode "key chord mode" :color red)
  ("l"     modi/linum-toggle "line numbers")
  ("m"     modi-mode "Modi mode")
  ("o"     modi/toggle-one-window "one window" :color red)
  ("p"     prez-mode "presentation mode")
  ("r"     dired-toggle-read-only "read only/editable") ; generalized `read-only-mode'
  ("sa"    scroll-all-mode "scroll all mode")
  ("t"     toggle-truncate-lines "truncate lines" :color red)
  ("C-t"   toggle-theme "dark/light theme")
  ("v"     view-mode "view mode") ; make buffer read-only, then use SPC/DEL to nav
  ("V"     visible-mode "visible mode") ; view all hidden text in org-mode (e.g. links)
  ("<SPC>" whitespace-mode "whitespace mode")
  ("q"     nil "cancel" :color blue))

(bind-key "s-t" #'hydra-toggle/body)
(bind-key "C-c t" #'hydra-toggle/body)


(provide 'setup-toggles)

;; http://endlessparentheses.com/the-toggle-map-and-wizardry.html
