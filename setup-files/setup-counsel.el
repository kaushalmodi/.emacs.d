;; Time-stamp: <2015-07-02 17:37:07 kmodi>

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
           ;; but still allow backup files named like abc.el~timestamp~
           "\\|\\(?:\\`[^~]+?[#~]\\'\\)"))

    (defun counsel-find-file ()
      "Forward to `find-file'."
      (interactive)
      (ivy-read "Find file: " 'read-file-name-internal
                :matcher #'counsel--find-file-matcher
                :action (cons
                         1
                         `(("default"
                            (lambda (x)
                              (find-file
                               (expand-file-name x ivy--directory))))
                           (,(propertize "delete"
                                         'face 'font-lock-warning-face)
                            (lambda (x)
                              (delete-file
                               (expand-file-name x ivy--directory))))))
                :preselect (when counsel-find-file-at-point
                             (require 'ffap)
                             (ffap-guesser))
                :require-match 'confirm-after-completion
                :history 'file-name-history
                :keymap counsel-find-file-map))

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
