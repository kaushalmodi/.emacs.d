;; Time-stamp: <2015-05-29 14:02:05 kmodi>

;; Ivy (comes packaged with the `swiper' package)

(use-package ivy
  :config
  (progn
    ;; Disable ido
    (when (boundp 'ido)
      (ido-mode -1))
    ;; Enable ivy
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)))


  (provide 'setup-ivy)
