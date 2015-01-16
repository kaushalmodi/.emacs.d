;; Time-stamp: <2015-01-16 10:08:25 kmodi>

;; YASnippet

(use-package yasnippet
  :config
  (progn
    (setq yas-prompt-functions '(yas-ido-prompt
                                 yas-completing-prompt)
          yas-new-snippet-default "# -*- mode: snippet -*-
# contributor: Kaushal Modi
# name: $1
# key: ${2:${1:$(yas--key-from-desc yas-text)}}${3:
# binding: ${4:direct-keybinding}}${5:
# expand-env: ((yas-indent-line 'auto) (yas-also-auto-indent-first-line t) (yas-wrap-around-region t))}
# --
$0")
    (define-key yas-minor-mode-map (kbd "TAB") nil)
    (define-key yas-minor-mode-map [(tab)] nil)
    (bind-keys
     :map modi-mode-map
     ("s-y s-y" . yas-expand)
     ("s-y y"   . yas-expand)
     ("s-y e"   . yas-expand)
     ("s-y i"   . yas-insert-snippet)
     ("s-y n"   . yas-new-snippet)
     ("s-y v"   . yas-visit-snippet-file)))
  :idle
  (progn
    ;; (yas-global-mode 1)
    (yas-reload-all)
    (dolist (hook '(verilog-mode-hook
                    emacs-lisp-mode-hook))
      (add-hook hook 'yas-minor-mode))))


(provide 'setup-yasnippet)
