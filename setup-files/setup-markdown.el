;; Time-stamp: <2013-12-02 17:07:00 kmodi>

;; Markdown / Pancake.io

(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

(provide 'setup-markdown)
