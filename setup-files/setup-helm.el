;; Time-stamp: <2014-03-14 09:27:06 kmodi>

;; Helm Swoop
;; Source: https://github.com/ShingoFukuyama/helm-swoop

(require 'helm-swoop)

;; Disable global helm-mode to stop it from intefering with `C-x C-f`
;; helm-swoop still works
(helm-mode -1)


(setq setup-helm-loaded t)
(provide 'setup-helm)
