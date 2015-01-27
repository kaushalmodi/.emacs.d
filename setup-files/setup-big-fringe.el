;; Time-stamp: <2015-01-23 16:57:32 kmodi>

;; Big Fringe (Minor Mode)
;; http://bzg.fr/emacs-strip-tease.html
(defvar bkp/fringe-indicator-alist fringe-indicator-alist
  "Backup of `fringe-indicator-alist'.")
(define-minor-mode bzg-big-fringe-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global t
  :lighter " zen"
  (if bzg-big-fringe-mode
      (progn
        (delete-other-windows) ; works only when one window is open
        (set-fringe-mode (/ (- (frame-pixel-width) (* 100 (frame-char-width))) 2))
        ;; (custom-set-faces '(fringe ((t (:background "#3F3F3F")))))
        (setq bkp/fringe-indicator-alist fringe-indicator-alist)
        (setq fringe-indicator-alist '((truncation nil nil) (continuation nil nil)))
        (setq indicate-buffer-boundaries nil)
        (modi/turn-off-fci-mode)
        (modi/turn-off-linum))
    (progn
      (set-fringe-style nil)
      ;; (custom-set-faces '(fringe ((t (:background (face-background 'default))))))
      (setq fringe-indicator-alist bkp/fringe-indicator-alist)
      (setq indicate-buffer-boundaries '((top . right) (bottom . right)))
      (modi/turn-on-fci-mode)
      (modi/turn-on-linum)
      (winner-undo))))

;; ;; Now activate this global minor mode
;; (bzg-big-fringe-mode 1)

;; Use a minimal cursor
;; (setq cursor-type 'hbar)

;; ;; Get rid of the indicators in the fringe
;; (mapcar (lambda(fb) (set-fringe-bitmap-face fb 'org-hide))
;;         fringe-bitmaps)


(provide 'setup-big-fringe)
