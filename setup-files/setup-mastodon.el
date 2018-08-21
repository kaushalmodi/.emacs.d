;; Time-stamp: <2018-08-21 14:34:15 kmodi>

;; Mastodon
;; https://github.com/jdenen/mastodon.el

(use-package mastodon
  :ensure t
  :defer t
  :config
  (progn
    (setq mastodon-instance-url "https://mastodon.technology")
    (setq mastodon-tl--enable-proportional-fonts t)

    (with-eval-after-load 'mastodon-tl
      (defun modi/mastodon-tl--byline (toot author-byline action-byline)
        "Generate byline for TOOT.

AUTHOR-BYLINE is function for adding the author portion of
the byline that takes one variable.
ACTION-BYLINE is a function for adding an action, such as boosting
favouriting and following to the byline. It also takes a single function. By default
it is `mastodon-tl--byline-boosted'"
        (let ((parsed-time (date-to-time (mastodon-tl--field 'created_at toot)))
              (faved (equal 't (mastodon-tl--field 'favourited toot)))
              (boosted (equal 't (mastodon-tl--field 'reblogged toot))))
          (concat
           (propertize "\n | " 'face 'default)
           (propertize
            (concat (when boosted
                      (format "(%s) "
                              (propertize "B" 'face 'mastodon-boost-fave-face)))
                    (when faved
                      (format "(%s) "
                              (propertize "F" 'face 'mastodon-boost-fave-face)))
                    (funcall author-byline toot)
                    (funcall action-byline toot)
                    " "
                    ;; TODO: Once we have a view for toot (responses etc.) make
                    ;; this a tab stop and attach an action.
                    (propertize
                     (format-time-string mastodon-toot-timestamp-format parsed-time)
                     'timestamp parsed-time
                     'display (if mastodon-tl--enable-relative-timestamps
                                  (mastodon-tl--relative-time-description parsed-time)
                                parsed-time))
                    ;; Tue Aug 21 14:27:57 EDT 2018 - kmodi
                    ;; Start of edit
                    ;; - Replaced "\n  ------------" with
                    ;;   "\n"
                    (propertize "\n" 'face 'default)
                    ;; End of edit
                    )
            'favourited-p faved
            'boosted-p    boosted
            'byline       t))))
      (advice-add 'mastodon-tl--byline :override #'modi/mastodon-tl--byline))

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
