;; Time-stamp: <2015-07-08 13:52:16 kmodi>

;; Artist Mode
(use-package artist
  :commands (artist-mode)
  :config
  (progn
    (use-package ascii-art-to-unicode
      :commands (aa2u aa2u-rectangle
                      aa2u-mark-as-text aa2u-mark-rectangle-as-text)
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
