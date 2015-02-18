;; Time-stamp: <2015-02-17 20:58:39 kmodi>

;; Hydra
;; https://github.com/abo-abo/hydra

(req-package hydra
  ;; :load-path "from-git/hydra"
  :config
  (progn
    (setq hydra-lv nil)
    (hydra-add-font-lock)))


(provide 'setup-hydra)
