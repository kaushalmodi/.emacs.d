;; Time-stamp: <2018-08-21 14:10:35 kmodi>

;; Mastodon
;; https://github.com/jdenen/mastodon.el

(use-package mastodon
  :ensure t
  :defer t
  :config
  (progn
    (setq mastodon-instance-url "https://mastodon.technology")

    (bind-keys
     :map mastodon-mode-map
     ("#" . mastodon-tl--get-tag-timeline)
     ("A" . mastodon-profile--get-toot-author)
     ("F" . mastodon-tl--get-federated-timeline)
     ("H" . mastodon-tl--get-home-timeline)
     ("L" . mastodon-tl--get-local-timeline)
     ("M-n" . mastodon-tl--next-tab-item)
     ("M-p" . mastodon-tl--previous-tab-item)
     ("N" . mastodon-notifications--get)
     ("P" . mastodon-profile--show-user)
     ("T" . mastodon-tl--thread)
     ("b" . mastodon-toot--toggle-boost)
     ("c" . mastodon-tl--toggle-spoiler-text-in-toot)
     ("f" . mastodon-toot--toggle-favourite)
     ("g" . mastodon-tl--update)
     ("k" . modi/kill-buffer-dwim)      ;Only kill
     ("n" . mastodon-tl--goto-next-toot)
     ("p" . mastodon-tl--goto-prev-toot)
     ("q" . modi/quit-and-kill-window)  ;Quit + kill
     ("r" . mastodon-toot--reply)
     ("t" . mastodon-toot)
     ("y" . bury-buffer)                ;Only bury
     ("z" . quit-window))))             ;Quit + bury

;; Emojify
;; https://github.com/iqbalansari/emacs-emojify
(use-package emojify                    ;Used by mastodon.el
  :ensure t
  :defer t)


(provide 'setup-mastodon)
