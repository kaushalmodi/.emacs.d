;; Time-stamp: <2014-03-07 15:13:15 kmodi>

;; magit
;; Source: https://github.com/magit/magit

(require 'magit)

(setq magit-completing-read-function 'magit-ido-completing-read
      magit-auto-revert-mode nil
      magit-repo-dirs '( "~/.emacs.d"
                         )
      )


(setq setup-magit-loaded t)
(provide 'setup-magit)
