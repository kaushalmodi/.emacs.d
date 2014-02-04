;; Time-stamp: <2013-12-12 16:27:03 kmodi>

;; YASnippet

(require 'yasnippet)
(yas-global-mode 1)

(setq yas-prompt-functions '(yas-ido-prompt
                             yas-completing-prompt)
      yas-new-snippet-default "# -*- mode: snippet -*-
# contributor: Kaushal Modi
# name: $1
# key: ${2:${1:$(yas--key-from-desc yas-text)}}${3:
# binding: ${4:direct-keybinding}}${5:
# expand-env: ((yas-indent-line 'auto) (yas-also-auto-indent-first-line t) (yas-wrap-around-region t))}
# --
$0"
      )


(setq setup-yasnippet-loaded t)
(provide 'setup-yasnippet)
