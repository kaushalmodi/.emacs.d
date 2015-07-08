;; Time-stamp: <2015-07-08 14:12:24 kmodi>

;; wrap-region
;; https://github.com/rejeep/wrap-region.el

(use-package wrap-region
  :config
  (progn
    ;; Enable `wrap-region' in the following major modes
    (dolist (hook '(emacs-lisp-mode-hook
                    org-mode-hook
                    text-mode-hook
                    markdown-mode-hook))
      (add-hook hook #'wrap-region-mode))

    (wrap-region-add-wrapper "`" "'" nil 'emacs-lisp-mode)

    (wrap-region-add-wrapper "`" "`"   nil '(text-mode markdown-mode))
    (wrap-region-add-wrapper "**" "**" "*" '(text-mode markdown-mode))
    (wrap-region-add-wrapper "*" "*"   "/" '(text-mode markdown-mode))
    (wrap-region-add-wrapper "~~" "~~" "+" '(text-mode markdown-mode))

    (wrap-region-add-wrapper "=" "=" nil 'org-mode)
    (wrap-region-add-wrapper "*" "*" nil 'org-mode)
    (wrap-region-add-wrapper "/" "/" nil 'org-mode)
    (wrap-region-add-wrapper "_" "_" nil 'org-mode)
    (wrap-region-add-wrapper "+" "+" nil 'org-mode)))


(provide 'setup-wrap-region)

;; (wrap-region-add-wrapper "<CHAR1>" "<CHAR2>" KEY 'MODE), or
;; (wrap-region-add-wrapper "<CHAR1>" "<CHAR2>" KEY '(MODE1 MODE2))
;; - Select TEXT, hit KEY (or <CHAR1> if KEY is nil), then TEXT becomes
;;   <CHAR1>TEXT<CHAR2> in major mode MODE (or in major modes MODE1 and MODE2)
