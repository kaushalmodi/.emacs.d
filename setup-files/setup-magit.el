;; Time-stamp: <2014-03-09 00:53:22 kmodi>

;; magit
;; Source: https://github.com/magit/magit

(require 'magit)

(setq magit-completing-read-function 'magit-ido-completing-read
      magit-auto-revert-mode nil
      magit-repo-dirs '( "~/.emacs.d"
                         )
      )

(magit-auto-revert-mode -1) ;; Disable magit auto revert

(setq setup-magit-loaded t)
(provide 'setup-magit)
