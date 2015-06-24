;; Time-stamp: <2015-06-23 14:27:31 kmodi>

;; Shackle
;; https://github.com/wasamasa/shackle

(use-package shackle
  :if (not (bound-and-true-p disable-pkg-shackle))
  :config
  (progn
    (setq shackle-lighter "")
    (setq shackle-select-reused-windows nil) ; default nil
    (setq shackle-default-alignment 'below) ; default below
    (setq shackle-default-ratio 0.4) ; default 0.5

    (setq shackle-rules
          ;; CONDITION(:regexp)           :select     :inhibit-window-quit   :ratio+:align|:other     :same|:popup
          '((compilation-mode             :select nil                                                )
            ("*undo-tree*"                                                   :ratio 0.25 :align right)
            ("*Shell Command Output*"     :select nil                                                )
            (occur-mode                   :select nil                                    :align t    )
            ("*Help*"                     :select t   :inhibit-window-quit t :other t                )
            ("*Completions*"                                                 :ratio 0.3  :align t    )
            ("*Messages*"                 :select t   :inhibit-window-quit t :other t                )
            ("\\*[Wo]*Man.*\\*"   :regexp t :select t   :inhibit-window-quit t :other t                )
            ("\\*poporg.*\\*"     :regexp t :select t                          :other t                )
            (".*COMMIT_EDITMSG" :regexp t :select t                          :other t                )
            ("\\`\\*helm.*?\\*\\'"  :regexp t                                    :ratio 0.3  :align t    )
            ))

    (shackle-mode 1)))


(provide 'setup-shackle)

;; Elements of the `shackle-rules' alist:
;;
;; |-----------+------------------------+--------------------------------------------------|
;; | CONDITION | symbol                 | Major mode of the buffer to match                |
;; |           | string                 | Name of the buffer                               |
;; |           |                        | - which can be turned into regexp matching       |
;; |           |                        | by using the :regexp key with a value of t       |
;; |           |                        | in the key-value part                            |
;; |           | list of either         | a list groups either symbols or strings          |
;; |           | symbol or string       | (as described earlier) while requiring at        |
;; |           |                        | least one element to match                       |
;; |           | t                      | t as the fallback rule to follow when no         |
;; |           |                        | other match succeeds.                            |
;; |           |                        | If you set up a fallback rule, make sure         |
;; |           |                        | it's the last rule in shackle-rules,             |
;; |           |                        | otherwise it will always be used.                |
;; |-----------+------------------------+--------------------------------------------------|
;; | KEY-VALUE | :select t              | Select the popped up window. The                 |
;; |           |                        | `shackle-select-reused-windows' option makes     |
;; |           |                        | this the default for windows already             |
;; |           |                        | displaying the buffer.                           |
;; |-----------+------------------------+--------------------------------------------------|
;; |           | :inhibit-window-quit t | Special buffers usually have `q' bound to        |
;; |           |                        | `quit-window' which commonly buries the buffer   |
;; |           |                        | and deletes the window. This option inhibits the |
;; |           |                        | latter which is especially useful in combination |
;; |           |                        | with :same, but can also be used with other keys |
;; |           |                        | like :other as well.                             |
;; |-----------+------------------------+--------------------------------------------------|
;; |           | :same t                | Display buffer in the current window.            |
;; |           | :popup t               | Pop up a new window instead of displaying        |
;; |           | *mutually exclusive*   | the buffer in the current one.                   |
;; |-----------+------------------------+--------------------------------------------------|
;; |           | :other t               | Reuse the window `other-window' would select if  |
;; |           | *must not be used      | there's more than one window open, otherwise pop |
;; |           | with :align, :ratio*   | up a new window. When used in combination with   |
;; |           |                        | the :frame key, do the equivalent to             |
;; |           |                        | other-frame or a new frame                       |
;; |-----------+------------------------+--------------------------------------------------|
;; |           | :align                 | Align a new window at the respective side of     |
;; |           | 'above, 'below,        | the current frame or with the default alignment  |
;; |           | 'left, 'right,         | (customizable with `shackle-default-alignment')  |
;; |           | or t (default)         | by deleting every other window than the          |
;; |           |                        | currently selected one, then wait for the window |
;; |           |                        | to be "dealt" with. This can either happen by    |
;; |           |                        | burying its buffer with q or by deleting its     |
;; |           |                        | window with C-x 0.                               |
;; |           | :ratio                 | Aligned window use a default ratio of 0.5 to     |
;; |           | a floating point       | split up the original window in half             |
;; |           | value between 0 and 1  | (customizable with `shackle-default-ratio'), the |
;; |           |                        | ratio can be changed on a per-case basis by      |
;; |           |                        | providing a different floating point value like  |
;; |           |                        | 0.33 to make it occupy a third of the original   |
;; |           |                        | window's size.                                   |
;; |-----------+------------------------+--------------------------------------------------|
;; |           | :frame t               | Pop buffer to a frame instead of a window.       |
;; |-----------+------------------------+--------------------------------------------------|

;; http://emacs.stackexchange.com/a/2195/115
;; While in "*magit..*" buffer on doing Commit (`c c'), the
;; ".. COMMIT_EDITMSG" buffer opens and reuses the *magit window.
;; This is not useful when you'd want to add details about what you
;; are committing while reviewing the diff in "*magit .." window.
;; So ensure that the ".. COMMIT_EDITMSG" buffer always pops up in
;; a new window.
