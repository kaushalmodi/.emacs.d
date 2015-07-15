;; Time-stamp: <2015-07-15 11:18:44 kmodi>

;; Which Key
;; https://github.com/justbur/emacs-which-key

(use-package which-key
  :defer 15
  :config
  (progn
    (setq which-key-popup-type 'minibuffer) ; default
    ;; (setq which-key-popup-type 'side-window)

    (which-key-mode 1)))


(provide 'setup-which-key)
