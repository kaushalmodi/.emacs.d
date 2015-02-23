;; Time-stamp: <2015-02-23 11:42:32 kmodi>

;; Markdown / Pancake.io

(use-package markdown-mode
  :commands (markdown-mode)
  :mode (("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))


(provide 'setup-markdown)
