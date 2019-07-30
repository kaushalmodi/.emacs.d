;; Time-stamp: <2018-08-27 16:14:28 kmodi>

;; Mastodon
;; https://github.com/jdenen/mastodon.el
(use-package mastodon
  :ensure t
  :defer t
  :config
  (progn
    (defconst modi/mastodon-account--me '((display_name . "My Profile")
                                          (id . "77333") ;https://mastodon.technology/web/accounts/77333
                                          (acct . "kaushalmodi")
                                          (note . ""))
      "Mastodon account alist used in `modi/mastodon-show-my-profile'.")

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
                    ;; - Replaced "\n  ------------" with "\n FORM FEED"
                    ;;   (Below, after \n, the invisible FORM FEED char is inserted using C-q C-l.)
                    (propertize "\n" 'face 'default)
                    ;; End of edit
                    )
            'favourited-p faved
            'boosted-p    boosted
            'byline       t))))
      (advice-add 'mastodon-tl--byline :override #'modi/mastodon-tl--byline))

    (defun modi/mastodon-toot-at-point ()
      "Return the toot at point as an alist.
If current toot is a Boosted toot, return the original toot."
      (let* ((this-toot (mastodon-tl--property 'toot-json))
             (orig-toot (cdr (assoc 'reblog this-toot))))
        (or orig-toot this-toot)))

    (defun modi/mastodon-get-url-of-toot-at-point ()
      "Return the URL of the toot at point as a string.
If the current toot is a Boosted toot, get the URL of the
original toot."
      (let ((toot (modi/mastodon-toot-at-point)))
        (cdr (assoc 'url toot))))

    (defun modi/mastodon-browse-url-of-toot-at-point ()
      "Browse the URL of the toot at point.
If the current toot is a Boosted toot, browse the URL of the
original toot."
      (interactive)
      (let ((url (modi/mastodon-get-url-of-toot-at-point)))
        (browse-url url)))

    (defun modi/mastodon-copy-url-of-toot-at-point ()
      "Copy the URL of the toot at point.
If the current toot is a Boosted toot, copy the URL of the
original toot."
      (interactive)
      (let ((url (modi/mastodon-get-url-of-toot-at-point)))
        (prog1
            (kill-new url)
          (message "Copied toot URL: %s" url))))

    (defun modi/mastodon-copy-text-of-toot-at-point ()
      "Copy the text of the toot at point.
If the current toot is a Boosted toot, copy the text of the
original toot."
      (interactive)
      (let* ((toot (modi/mastodon-toot-at-point))
             (url (modi/mastodon-get-url-of-toot-at-point))
             (content (cdr (assoc 'content toot)))
             (text (mastodon-tl--render-text content toot)))
        (prog1
            (kill-new text)
          (message "Copied text from toot %s" url))))

    (defun modi/mastodon-scroll-up ()
      "Scroll up in the Mastodon buffer.
If end of buffer is reached, call `mastodon-tl--more'."
      (interactive)
      (let ((scroll-error-top-bottom t))
        (condition-case nil
            (scroll-up-command)
          (error
           (mastodon-tl--more)
           ;; Move the older end of buffer to window-line 10
           ;; (arbitrarily picked).
           (recenter-top-bottom 10)))))

    (defun modi/mastodon-show-my-profile ()
      "Open Mastodon buffer for my profile."
      (interactive)
      (require 'mastodon-profile)
      (cl-letf (((symbol-function #'mastodon-profile--image-from-account)
                 (lambda (_) "")))           ;No need to load my own image
        (mastodon-profile--make-profile-buffer-for
         modi/mastodon-account--me "statuses" #'mastodon-tl--timeline)))

    (bind-keys
     :map mastodon-mode-map
     ("#" . mastodon-tl--get-tag-timeline)
     ("A" . mastodon-profile--get-toot-author)
     ("F" . mastodon-tl--get-federated-timeline)
     ("H" . mastodon-tl--get-home-timeline)
     ("L" . mastodon-tl--get-local-timeline)
     ("M" . modi/mastodon-show-my-profile)
     ("M-n" . mastodon-tl--next-tab-item)
     ("M-p" . mastodon-tl--previous-tab-item)
     ("N" . mastodon-notifications--get)
     ("P" . mastodon-profile--show-user)
     ("SPC" . modi/mastodon-scroll-up)
     ("T" . mastodon-tl--thread)
     ("W" . modi/mastodon-copy-text-of-toot-at-point)
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
     ("v" . modi/mastodon-browse-url-of-toot-at-point)
     ("w" . modi/mastodon-copy-url-of-toot-at-point)
     ("y" . bury-buffer)                ;Only bury
     ("z" . quit-window))))             ;Quit + bury

;; Emojify
;; https://github.com/iqbalansari/emacs-emojify
(use-package emojify                    ;Used by mastodon.el
  :ensure t
  :defer t)


(provide 'setup-mastodon)
