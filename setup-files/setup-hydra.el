;; Time-stamp: <2015-02-24 08:48:21 kmodi>

;; Hydra
;; https://github.com/abo-abo/hydra

(use-package hydra
  ;; :load-path "elisp/hydra"
  :config
  (progn
    ;; (setq hydra-lv nil)
    (hydra-add-font-lock)))


(provide 'setup-hydra)

;; |----------+-----------+------------------+----------------+-------------|
;; | Body     | Head      | Allows execution | Quits hydra    | Quits hydra |
;; | Color    | Inherited | of NON-HEADs     | after NON-HEAD | after HEAD  |
;; |          | Color     |                  | execution      | execution   |
;; |----------+-----------+------------------+----------------+-------------|
;; | amaranth | red       | No               | No             | No          |
;; | pink     | red       | Yes              | No             | No          |
;; | red      | red       | Yes              | Yes            | No          |
;; | teal     | blue      | No               | No             | Yes         |
;; | blue     | blue      | Yes              | Yes            | Yes         |
;; |----------+-----------+------------------+----------------+-------------|
