;; Time-stamp: <2015-09-02 16:31:01 kmodi>

;; Matlab

(use-package matlab-load
  :load-path "elisp/matlab-emacs"
  :mode (("\\.m\\'" . matlab-mode))
  :config
  (progn
    (with-eval-after-load 'matlab
      ;; Prevent conflict with the emacs default `search-map' binding `M-s'
      (define-key matlab-mode-map (kbd "M-s") nil))))


(provide 'setup-matlab)
