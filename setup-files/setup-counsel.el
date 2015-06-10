;; Time-stamp: <2015-06-10 10:25:02 kmodi>

;; Counsel (comes packaged with the `swiper' package)

(use-package counsel
  :config
  (progn
    (setq counsel-find-file-at-point t)
    (setq counsel-find-file-ignore-regexp
          (concat
           ;; file names beginning with # or .
           "\\(?:\\`[#.]\\)"
           ;; file names ending with # or ~
           ;; but still allow backup files named like abc.el~timestamp~
           "\\|\\(?:\\`[^~]+?[#~]\\'\\)"))
    (bind-keys
     :map modi-mode-map
      ("C-x C-f" . counsel-find-file)
      ("C-h v"   . counsel-describe-variable)
      ("C-h f"   . counsel-describe-function)
      ("C-c u"   . counsel-unicode-char))
    (key-chord-define-global ";'" #'counsel-M-x)))


(provide 'setup-counsel)

;; Peek at files with `C-M-n' and `C-M-p'
;; Input a leading dot to see all files
;;
;; Related blog posts:
;; - http://oremacs.com/2015/06/08/describe-variable-tip
