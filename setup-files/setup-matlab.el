;; Time-stamp: <2015-11-04 10:57:22 kmodi>

;; Matlab

(use-package matlab-load
  :load-path "elisp/matlab-emacs"
  :mode (("\\.m\\'" . matlab-mode))
  :config
  (progn
    (with-eval-after-load 'matlab
      ;; Prevent conflict with the emacs default `search-map' binding `M-s'
      (bind-key "M-s" nil matlab-mode-map))))


(provide 'setup-matlab)
