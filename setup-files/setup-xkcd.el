;; Time-stamp: <2014-03-02 11:11:06 kmodi>

;; xkcd

(require 'xkcd)

;; create the xkcd directory if it doesn't exist
(setq xkcd-dir (concat user-emacs-directory "/xkcd"))
(unless (file-exists-p xkcd-dir)
  (make-directory xkcd-dir))

(define-key xkcd-mode-map (kbd "C-p") 'xkcd-prev)
(define-key xkcd-mode-map (kbd "C-n") 'xkcd-next)
;; (define-key xkcd-mode-map (kbd "r") 'xkcd-rand)
;; (define-key xkcd-mode-map (kbd "t") 'xkcd-alt-text)
;; (define-key xkcd-mode-map (kbd "q") 'xkcd-kill-buffer)
;; (define-key xkcd-mode-map (kbd "o") 'xkcd-open-browser)
;; (define-key xkcd-mode-map (kbd "e") 'xkcd-open-explanation-browser)


(setq setup-xkcd-loaded t)
(provide 'setup-xkcd)
