;; Time-stamp: <2015-06-10 13:19:00 kmodi>

;; Ivy (comes packaged with the `swiper' package)

(use-package ivy
  :if (not (bound-and-true-p disable-pkg-ivy))
  :config
  (progn
    ;; Disable ido
    (when (boundp 'ido)
      (ido-mode -1))
    ;; Enable ivy
    (ivy-mode 1)

    (setq ivy-use-virtual-buffers t)

    ;; (setq ivy-re-builders-alist '((t . ivy--regex-plus))) ; default
    (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))

    ;; overriding the `C-x C-o` binding with `delete-blank-lines'
    (bind-key "C-x C-o" #'ivy-recentf modi-mode-map)
    ;; Revert the default bindings to C-j and C-m
    (bind-keys
     :map ivy-minibuffer-map
      ("C-m" . ivy-alt-done) ; RET
      ("C-j" . ivy-done))))


(provide 'setup-ivy)
