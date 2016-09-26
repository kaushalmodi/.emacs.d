;; Time-stamp: <2016-09-23 13:14:12 kmodi>

;; Matlab

(use-package matlab-load
  :load-path "elisp/matlab-emacs"
  :mode (("\\.m\\'" . matlab-mode))
  :commands (matlab-shell)
  :config
  (progn
    (with-eval-after-load 'matlab
      ;; Prevent conflict with the emacs default `search-map' binding `M-s'
      (bind-key "M-s" nil matlab-mode-map))))


(provide 'setup-matlab)
