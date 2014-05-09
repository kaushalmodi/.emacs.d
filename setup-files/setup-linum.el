;; Time-stamp: <2014-05-04 22:25:59 kmodi>

;; NLinum

(require 'nlinum)
;; Enable the line number column on the left
(global-nlinum-mode -1)
(setq nlinum-format "%4d ") ; right aligned, 4 char wide line num col

;; disable linum for selected modes
;; (specified in `linum-disabled`modes-list` in linum-off.el)
;; (require 'linum-off)

;; Turn on nlinum only in the specified modes
(add-hook 'verilog-mode-hook    'nlinum-mode)
(add-hook 'sh-mode-hook         'nlinum-mode)
(add-hook 'emacs-lisp-mode-hook 'nlinum-mode)
(add-hook 'cperl-mode-hook      'nlinum-mode)
(add-hook 'matlab-mode-hook     'nlinum-mode)
(add-hook 'python-mode-hook     'nlinum-mode)


(setq setup-linum-loaded t)
(provide 'setup-linum)
