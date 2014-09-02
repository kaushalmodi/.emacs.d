;; Time-stamp: <2014-08-13 11:45:18 kmodi>

;; Markdown / Pancake.io

(req-package markdown-mode
  :commands (markdown-mode)
  :mode (("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))


(provide 'setup-markdown)
