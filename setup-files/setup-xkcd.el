;; Time-stamp: <2016-05-19 22:07:17 kmodi>

;; xkcd
;; https://github.com/vibhavp/emacs-xkcd

(use-package xkcd
  :defer t
  :config
  (progn
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
