;; Time-stamp: <2015-08-19 14:08:27 kmodi>

;; Hardcore mode
;; https://github.com/magnars/hardcore-mode.el

(use-package hardcore-mode
  :if (not (bound-and-true-p disable-pkg-hardcore-mode))
  :init
  (progn
    (setq too-hardcore-backspace t)
    (setq too-hardcore-return    t))
  :config
  (progn
    (global-hardcore-mode 1)))


(provide 'setup-hardcore)
