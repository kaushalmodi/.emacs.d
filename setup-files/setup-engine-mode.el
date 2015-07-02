;; Time-stamp: <2015-07-02 14:07:19 kmodi>

;; Engine mode
;; https://github.com/hrs/engine-mode

(use-package engine-mode
  :config
  (progn
    (setq engine/browser-function 'eww-browse-url)

    (engine/set-keymap-prefix (kbd "C-c /"))

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

    (defengine word
      "http://wordnik.com/words/%s"
      :term-transformation-hook downcase
      :keybinding "w")

    (defengine wikipedia
      "https://en.wikipedia.org/wiki/%s"
      :keybinding "W")

    (engine-mode t)))


(provide 'setup-engine-mode)
