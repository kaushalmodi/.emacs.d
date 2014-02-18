;; Time-stamp: <2014-02-18 13:26:32 kmodi>

;; magit
;; Source: https://github.com/magit/magit

(require 'magit)

(setq magit-completing-read-function 'magit-ido-completing-read
      magit-repo-dirs '( "~/.emacs.d"
                         )
      )


(setq setup-magit-loaded t)
(provide 'setup-magit)
