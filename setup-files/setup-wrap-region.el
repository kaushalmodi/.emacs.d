;; Time-stamp: <2014-10-24 10:57:53 kmodi>

;; wrap-region
;; https://github.com/rejeep/wrap-region.el

(req-package wrap-region
  :config
  (progn
    ;; Enable wrap-region in the following major modes
    (dolist (hook '(emacs-lisp-mode-hook
                    org-mode-hook))
      (add-hook hook 'wrap-region-mode))

    (wrap-region-add-wrapper "`" "'") ; select region, hit ` then region -> `region'

    (wrap-region-add-wrapper "=" "=" nil 'org-mode) ; select region, hit = then region -> =region= in org-mode
    (wrap-region-add-wrapper "*" "*" nil 'org-mode) ; select region, hit * then region -> *region* in org-mode
    (wrap-region-add-wrapper "/" "/" nil 'org-mode) ; select region, hit / then region -> /region/ in org-mode
    (wrap-region-add-wrapper "_" "_" nil 'org-mode) ; select region, hit _ then region -> _region_ in org-mode
    (wrap-region-add-wrapper "+" "+" nil 'org-mode))) ; select region, hit + then region -> +region+ in org-mode


(provide 'setup-wrap-region)
