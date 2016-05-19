;; Time-stamp: <2016-05-19 18:26:10 kmodi>

;; Artist Mode
(use-package artist
  :defer t
  :config
  (progn
    (defun modi/artist-mode-hook-fn ()
      (if artist-mode
          (message "Artist mode is now enabled. Hit `C-c C-a' prefix to start drawing!")
        (message "Artist mode is disabled.")))
    (add-hook 'artist-mode-hook #'modi/artist-mode-hook-fn)

    ;; http://www.gnuvola.org/software/aa2u/
    (use-package ascii-art-to-unicode
      :defer t
      :config
      (progn
        (setq aa2u-uniform-weight 'LIGHT))))) ; values: 'LIGHT, 'HEAVY


(provide 'setup-artist)

;; ascii-art-to-unicode example use case

;; - M-x artist-mode RET
;; - C-c C-a r               ; artist-select-op-rectangle
;; - (draw two rectangles)

;; +---------------+
;; |               |
;; |       +-------+--+
;; |       |       |  |
;; |       |       |  |
;; |       |       |  |
;; +-------+-------+  |
;;         |          |
;;         |          |
;;         |          |
;;         +----------+

;; - C-c C-c                 ; artist-mode-off (optional)
;; - C-x n n                 ; narrow-to-region
;; - M-x aa2u RET

;; ┌───────────────┐
;; │               │
;; │       ┌───────┼──┐
;; │       │       │  │
;; │       │       │  │
;; │       │       │  │
;; └───────┼───────┘  │
;;         │          │
;;         │          │
;;         │          │
;;         └──────────┘
