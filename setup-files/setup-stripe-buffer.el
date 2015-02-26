;; Time-stamp: <2015-02-26 12:00:16 kmodi>

;; Stripe Mode
;; https://github.com/sabof/stripe-buffer

(use-package stripe-buffer
    :config
  (progn
    ;; How can I override a pre-defined face for light and dark backgrounds?
    ;; http://emacs.stackexchange.com/q/9600/115
    (defface my/stripe-hl-line
      '((((background dark))  (:overline "gray" :underline "gray" :foreground "dodger blue"))
        (t                    (:overline "gray" :underline "gray" :foreground "red")))
      "Bold face for highlighting the current line in Hl-Line mode."
      :group 'stripe-buffer)

    (defun my/stripe-hl-line-face-remap ()
      (face-remap-add-relative 'stripe-hl-line 'my/stripe-hl-line))

    (advice-add 'stripe-listify-buffer :after #'my/stripe-hl-line-face-remap)

    (add-hook 'package-menu-mode-hook #'stripe-listify-buffer)
    ;; (add-hook 'dired-mode-hook #'stripe-listify-buffer)

    (add-hook 'org-mode-hook          #'turn-on-stripe-table-mode)))


(provide 'setup-stripe-buffer)
