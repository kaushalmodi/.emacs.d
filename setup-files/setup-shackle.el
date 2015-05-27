;; Time-stamp: <2015-05-27 14:38:50 kmodi>

;; Shackle
;; https://github.com/wasamasa/shackle

(use-package shackle
  :if (not (bound-and-true-p disable-pkg-shackle))
  :config
  (progn
    (setq shackle-select-reused-windows nil) ; default nil
    (setq shackle-default-alignment 'below) ; default below
    (setq shackle-default-ratio 0.4) ; default 0.5

    (setq shackle-rules
          '((compilation-mode                :select nil)
            ("*undo-tree*"                               :ratio 0.25 :align right )
            ("*Shell Command Output*"        :select nil)
            (occur-mode                      :select nil             :align t)
            ("*Help*"                        :select t   :ratio 0.3  :align t)
            ("*Completions*"                             :ratio 0.3  :align t)
            ("\\`\\*helm.*?\\*\\'" :regexp t             :ratio 0.3  :align t)))

    (shackle-mode 1)))


(provide 'setup-shackle)

;; Elements of the `shackle-rules' alist:
;;
;; |-----------+-----------------------+--------------------------------------------------|
;; | CONDITION | symbol                | Major mode of the buffer to match                |
;; |           | string                | Name of the buffer                               |
;; |           |                       | - which can be turned into regexp matching       |
;; |           |                       | by using the :regexp key with a value of t       |
;; |           |                       | in the key-value part                            |
;; |           | list of either        | a list groups either symbols or strings          |
;; |           | symbol or string      | (as described earlier) while requiring at        |
;; |           |                       | least one element to match                       |
;; |           | t                     | t as the fallback rule to follow when no         |
;; |           |                       | other match succeeds.                            |
;; |           |                       | If you set up a fallback rule, make sure         |
;; |           |                       | it's the last rule in shackle-rules,             |
;; |           |                       | otherwise it will always be used.                |
;; |-----------+-----------------------+--------------------------------------------------|
;; | KEY-VALUE | :select t             | Select the popped up window. The                 |
;; |           |                       | `shackle-select-reused-windows' option makes     |
;; |           |                       | this the default for windows already             |
;; |           |                       | displaying the buffer.                           |
;; |           | :same t               | Display buffer in the current window.            |
;; |           | :popup t              | Pop up a new window instead of displaying        |
;; |           |                       | the buffer in the current one.                   |
;; |           | :align 'above,        | Align a new window at the respective side of     |
;; |           | 'below, 'left,        | the current frame or with the default alignment  |
;; |           | right, or t (default) | (customizable with `shackle-default-alignment')  |
;; |           |                       | by deleting every other window than the          |
;; |           |                       | currently selected one, then wait for the window |
;; |           |                       | to be "dealt" with. This can either happen by    |
;; |           |                       | burying its buffer with q or by deleting its     |
;; |           |                       | window with C-x 0.                               |
;; |           | :ratio                | Aligned window use a default ratio of 0.5 to     |
;; |           | a floating point      | split up the original window in half             |
;; |           | value between 0 and 1 | (customizable with `shackle-default-ratio'), the |
;; |           |                       | ratio can be changed on a per-case basis by      |
;; |           |                       | providing a different floating point value like  |
;; |           |                       | 0.33 to make it occupy a third of the original   |
;; |           |                       | window's size.                                   |
;; |           | :frame t              | Pop buffer to a frame instead of a window.       |
;; |-----------+-----------------------+--------------------------------------------------|
;;
