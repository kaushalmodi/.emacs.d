;; Time-stamp: <2014-08-13 10:38:11 kmodi>

;; Elisp Slime Nav
;; gtags/ctags like navigation into elisp source codes (even the compressed ones)
;; Source: https://github.com/purcell/elisp-slime-nav

(req-package elisp-slime-nav
  :config
  (progn
    (dolist (hook '(emacs-lisp-mode-hook
                    ielm-mode-hook))
      (add-hook hook 'elisp-slime-nav-mode))))


(provide 'setup-elisp-slime-nav)


;; elisp-slime-nav supports navigation to the definitions of variables,
;; functions, libraries and faces.
;; M-.                   <- Navigate to the symbol at point
;; M-,                   <- Pop back to previous marks
;; C-c C-d d/C-c C-d C-d <- Describe the symbol at point, whatever its type.
