;; Time-stamp: <2016-05-19 22:29:29 kmodi>

;; All
;; http://elpa.gnu.org/packages/all.html
(use-package all
  :defer t
  :init
  (progn
    (bind-to-modi-map ":" #'all))
  :config
  (progn
    (with-eval-after-load 'setup-windows-buffers ; for `modi/kill-buffer-dwim'
      (bind-keys
       :map all-mode-map
        ("C-c C-c" . all-mode-goto)
        ("C-c C-k" . modi/kill-buffer-dwim)))

    ;; All extension
    ;; http://www.emacswiki.org/emacs/download/all-ext.el
    (use-package all-ext)))


(provide 'setup-all)

;; `all-ext' enables `next-error' and `previous-error' navigation in the *All*
;; buffer using `M-g M-n' and `M-g M-p' while the point is in the file buffer
;; (like Occur)

;; Note that you cannot UNDO in the *All* buffer!
