;; Time-stamp: <2014-08-13 09:28:02 kmodi>

;; xkcd

(req-package xkcd
  :commands (xkcd)
  :config
  (progn
    ;; create the xkcd directory if it doesn't exist
    (setq xkcd-dir (concat user-emacs-directory "/xkcd"))
    (unless (file-exists-p xkcd-dir)
      (make-directory xkcd-dir))
    (bind-keys
     :map xkcd-mode-map
    ("C-p" . xkcd-prev)
    ("C-n" . xkcd-next)
    ("r"   . xkcd-rand)
    ("t"   . xkcd-alt-text)
    ("q"   . xkcd-kill-buffer)
    ("o"   . xkcd-open-browser)
    ("e"   . xkcd-open-explanation-browser))))


(provide 'setup-xkcd)
