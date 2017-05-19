;; Time-stamp: <2017-05-19 16:45:41 kmodi>

;; Engine mode
;; https://github.com/hrs/engine-mode

(use-package engine-mode
  :init
  (progn
    ;; I don't want to override the `org-sparse-tree' binding (C-c /) in org-mode.
    (setq engine/keybinding-prefix "C-x /"))
  :config
  (progn
    (setq engine/browser-function 'eww-browse-url)

    (defengine duckduckgo
      "https://duckduckgo.com/html/?q=%s"
      :keybinding "d")

    (defengine github-el
      "https://github.com/search?type=Code&q=extension:el+%s"
      :keybinding "e"
      :browser 'browse-url-firefox
      :docstring "Search .el files on github.com.")

    (defengine google
      "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
      :keybinding "g")

    (defengine github-el-km
      "https://github.com/search?type=Code&q=extension:el+user:kaushalmodi+%s"
      :keybinding "m"
      :browser 'browse-url-firefox
      :docstring "Search .el files on github.com/kaushalmodi.")

    (defengine wikipedia
      "https://en.wikipedia.org/wiki/%s"
      :keybinding "p")                  ;wiki(p)edia

    (defengine emacs-devel
      "https://lists.gnu.org/archive/cgi-bin/namazu.cgi?query=%s&submit=Search&idxname=emacs-devel"
      :keybinding "v"                   ;emacs de(v)el
      :browser 'browse-url-firefox
      :docstring "Search posts on emacs-devel archive.")

    (defengine word
      "http://wordnik.com/words/%s"
      :term-transformation-hook downcase
      :keybinding "w")

    (engine-mode 1)))


(provide 'setup-engine-mode)
