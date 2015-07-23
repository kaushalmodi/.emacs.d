;; Time-stamp: <2015-07-23 09:20:18 kmodi>

;; Counsel (comes packaged with the `swiper' package)

(use-package counsel
  :if (not (bound-and-true-p disable-pkg-ivy))
  :config
  (progn
    (setq counsel-find-file-at-point t)
    (setq counsel-find-file-ignore-regexp
          (concat
           ;; file names beginning with # or .
           "\\(?:\\`[#.]\\)"
           ;; file names ending with # or ~
           "\\|\\(?:\\`.+?[#~]\\'\\)"))

    (ivy-set-actions
     'counsel-find-file
     `(("x"
        (lambda (x) (delete-file (expand-file-name x ivy--directory)))
        ,(propertize "delete" 'face 'font-lock-warning-face))))

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
