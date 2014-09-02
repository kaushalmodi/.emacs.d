;; Time-stamp: <2014-08-13 11:31:43 kmodi>

;; Helm Swoop
;; Source: https://github.com/ShingoFukuyama/helm-swoop

(req-package helm-swoop
  :config
  (progn
    ;; Disable global helm-mode to stop it from intefering with `C-x C-f`
    ;; helm-swoop still works
    (helm-mode -1)))


(provide 'setup-helm)

;; Commented out below; now replaced with swoop, thus not needing to install
;; helm anymore!
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; helm-swoop package
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (when (boundp 'setup-helm-loaded)
;;   (global-set-key (kbd "M-i") 'helm-swoop)
;;   (global-set-key (kbd "M-I") 'helm-multi-swoop-all)
;;   ;; (global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
;;   ;; When doing isearch, hand the word over to helm-swoop
;;   (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch))
