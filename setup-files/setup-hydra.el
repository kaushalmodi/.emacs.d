;; Time-stamp: <2015-02-17 19:24:06 kmodi>

;; Hydra
;; https://github.com/abo-abo/hydra

(use-package hydra
  ;; :load-path "from-git/hydra"
  :config
  (progn
    (setq hydra-lv nil)
    (hydra-add-font-lock)))


(provide 'setup-hydra)
