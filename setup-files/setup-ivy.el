;; Time-stamp: <2015-06-04 15:05:51 kmodi>

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
        (bind-key "C-x C-f" #'counsel-find-file modi-mode-map)))
    ;; overriding the `C-x C-o` binding with `delete-blank-lines'
    (bind-key "C-x C-o" #'ivy-recentf modi-mode-map)))


(provide 'setup-ivy)

;; Counsel
;; Peek at files with "C-M-n" and "C-M-p". Input a leading dot to see all files.
