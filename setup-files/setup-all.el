;; Time-stamp: <2015-09-13 23:54:48 kmodi>

;; All
;; http://elpa.gnu.org/packages/all.html
(use-package all
  :commands (all)
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
