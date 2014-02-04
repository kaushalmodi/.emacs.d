;; Time-stamp: <2013-12-12 14:45:51 kmodi>

;; Helm Swoop
;; Source: https://github.com/ShingoFukuyama/helm-swoop

;; ;; helm from https://github.com/emacs-helm/helm
;; (require 'helm-config)
;; (helm-mode 1)

;; (setq helm-ff-auto-update-initial-value nil
;;       )

(require 'helm-swoop)

;; Disable global helm-mode to stop it from intefering with `C-x C-f`
;; helm-swoop still works
(helm-mode -1)


(setq setup-helm-loaded t)
(provide 'setup-helm)
