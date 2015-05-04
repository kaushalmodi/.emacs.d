;; Time-stamp: <2015-05-04 14:46:16 kmodi>

;; Matlab

(use-package matlab-load
  :load-path "elisp/matlab-emacs"
  :config
  (progn
    (with-eval-after-load 'matlab
      ;; Prevent conflict with the emacs default `search-map' binding `M-s'
      (define-key matlab-mode-map (kbd "M-s") nil))))


(provide 'setup-matlab)
