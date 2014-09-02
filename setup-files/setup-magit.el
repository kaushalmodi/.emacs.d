;; Time-stamp: <2014-08-18 13:25:47 kmodi>

;; magit
;; Source: https://github.com/magit/magit

(req-package magit
  :commands (magit-status)
  :requires (key-chord)
  :init
  (progn
    (bind-keys
     :map modi-mode-map
     ("<f11>"   . magit-status)
     ("<S-f11>" . magit-push)
     ("<M-f11>" . magit-pull))
    (key-chord-define-global "-[" 'magit-status)) ;; alternative for F11
  :config
  (progn
    (setq magit-completing-read-function 'magit-ido-completing-read
          magit-auto-revert-mode nil
          magit-repo-dirs '( "~/.emacs.d"))
    (magit-auto-revert-mode -1))) ;; Disable magit auto revert


(provide 'setup-magit)
