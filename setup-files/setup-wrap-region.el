;; Time-stamp: <2014-06-18 16:54:52 kmodi>

;; wrap-region
;; https://github.com/rejeep/wrap-region.el

(require 'wrap-region)

;; Enable wrap-region in the following major modes
(add-hook 'org-mode-hook        'wrap-region-mode)
(add-hook 'emacs-lisp-mode-hook 'wrap-region-mode)

(wrap-region-add-wrapper "`" "'") ; hit ` then region -> `region'

;; Note that the '=' wrapping won't work if expand-region is active!
(wrap-region-add-wrapper "=" "=" nil 'org-mode) ; hit $ then region -> =region= in org-mode

(wrap-region-add-wrapper "*" "*" nil 'org-mode) ; hit $ then region -> *region* in org-mode
(wrap-region-add-wrapper "/" "/" nil 'org-mode) ; hit $ then region -> /region/ in org-mode
(wrap-region-add-wrapper "_" "_" nil 'org-mode) ; hit $ then region -> _region_ in org-mode
(wrap-region-add-wrapper "+" "+" nil 'org-mode) ; hit $ then region -> +region+ in org-mode


(setq setup-wrap-region-loaded t)
(provide 'setup-wrap-region)
