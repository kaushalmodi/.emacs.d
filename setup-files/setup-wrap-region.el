;; Time-stamp: <2015-10-12 12:19:29 kmodi>

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

    ;; Override the default `wrap-region-define-wrappers' function so that it
    ;; does not bind the "[", "{", "<" keys each time `wrap-region-mode' is
    ;; enabled in a buffer.
    (defun wrap-region-define-wrappers ()
      "Defines defaults wrappers."
      (mapc
       (lambda (pair)
         (apply 'wrap-region-add-wrapper pair))
       '(("\"" "\"")
         ("'"  "'")
         ("("  ")")))
      ;; Unbind the wrap region pairs which I am very unlikely to use.
      ;; Doing so allows me to bind those to more useful functions in
      ;; `region-bindings-mode-map'. See `setup-multiple-cursors.el' file
      ;; for examples.
      (wrap-region-unset-key "[")
      (wrap-region-unset-key "{")
      (wrap-region-unset-key "<"))

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
