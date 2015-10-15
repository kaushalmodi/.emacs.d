;; Time-stamp: <2015-10-15 14:24:12 kmodi>

;; xkcd
;; https://github.com/vibhavp/emacs-xkcd

(use-package xkcd
  :commands (xkcd)
  :init
  (progn
    (setq xkcd-cache-dir (let ((dir (concat user-emacs-directory
                                            "xkcd/"))) ; must end with /
                           (make-directory dir :parents)
                           dir)))
  :config
  (progn
    (defun xkcd-copy-link ()
      "Save the link to the current comic to the kill-ring."
      (interactive)
      (let ((link (concat "http://xkcd.com/"
                          (number-to-string xkcd-cur))))
        (kill-new link)
        (message link)))

    (bind-keys
     :map xkcd-mode-map
      ("/" . xkcd-get)
      ("g" . xkcd-get-latest)
      ("c" . xkcd-get-latest-cached)
      ("p" . xkcd-prev)
      ("n" . xkcd-next)
      ("v" . xkcd-open-browser)
      ("w" . xkcd-copy-link)
      ("?" . xkcd-open-explanation-browser))))


(provide 'setup-xkcd)

;; |---------+--------------------------------|
;; | Binding | Description                    |
;; |---------+--------------------------------|
;; | /       | Go to a specific comic         |
;; | g       | Get the latest comic (refresh) |
;; | c       | Get the latest cached comic    |
;; | p       | Previous comic                 |
;; | n       | Next comic                     |
;; | r       | Random                         |
;; | v       | Open in browser                |
;; | w       | Copy link                      |
;; | ?       | Explain the comic              |
;; | t       | Show alt text                  |
;; | q       | Quit                           |
;; |---------+--------------------------------|
