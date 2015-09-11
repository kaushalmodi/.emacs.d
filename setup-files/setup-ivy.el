;; Time-stamp: <2015-09-04 15:22:01 kmodi>

;; Ivy (comes packaged with the `swiper' package)

(use-package ivy
  :if (not (bound-and-true-p disable-pkg-ivy))
  :config
  (progn
    (when (not (bound-and-true-p disable-pkg-ivy))
      ;; Disable ido
      (with-eval-after-load 'ido
        (ido-mode -1))
      ;; Enable ivy
      (ivy-mode 1))

    (setq ivy-display-style nil) ; default nil
    (setq ivy-use-virtual-buffers t)
    (setq ivy-count-format "%d/%d ")
    (setq ivy-re-builders-alist '((t . ivy--regex-plus))) ; default
    ;; (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))

    (use-package ivy-hydra
      :config
      (progn
        (defhydra hydra-ivy (:hint nil
                             :color pink)
          "
^^_,_      _f_ollow  _i_nsert  ^^_c_alling %s(if ivy-calling \"on\" \"off\")  _w_/_s_: %s(ivy-action-name)
_p_/_n_    _d_one    _q_uit    ^^_m_atcher %s(if (eq ivy--regex-function 'ivy--regex-fuzzy) \"fuzzy\" \"ivy\")
^^_._      _D_o it!  ^^        _<_/_>_ shrink/grow window
"
          ;; arrows
          (","   ivy-beginning-of-buffer) ; default h
          ("p"   ivy-previous-line) ; default j
          ("n"   ivy-next-line) ; default k
          ("."   ivy-end-of-buffer) ; default l
          ;; actions
          ("f"   ivy-alt-done         :exit nil)
          ("C-m" ivy-alt-done         :exit nil) ; RET ; default C-j
          ("C-j" ivy-done             :exit t) ; default C-m
          ("d"   ivy-done             :exit t)
          ("g"   ivy-call)
          ("D"   ivy-immediate-done   :exit t)
          ("c"   ivy-toggle-calling)
          ("m"   ivy-toggle-fuzzy)
          (">"   ivy-minibuffer-grow)
          ("<"   ivy-minibuffer-shrink)
          ("w"   ivy-prev-action)
          ("s"   ivy-next-action)
          ;; quit hydra
          ("i"   nil)
          ("C-o" nil)
          ;; quit ivy
          ("q"   keyboard-escape-quit :exit t); default o
          ("C-g" keyboard-escape-quit :exit t))))

    ;; Revert the default bindings to C-j and C-m
    (bind-keys
     :map ivy-minibuffer-map
      ("C-m"   . ivy-alt-done) ; RET
      ("C-S-m" . ivy-immediate-done)
      ("C-j"   . ivy-done)
      ("C-t"   . ivy-toggle-fuzzy)
      ("C-o"   . hydra-ivy/body))
    (key-chord-define ivy-minibuffer-map "m," #'ivy-beginning-of-buffer)
    (key-chord-define ivy-minibuffer-map ",." #'ivy-end-of-buffer)

    ;; https://github.com/abo-abo/swiper/issues/164
    (defun modi/ivy-kill-buffer ()
      (interactive)
      (ivy-set-action 'kill-buffer)
      (ivy-done))
    (bind-keys
     :map ivy-switch-buffer-map
      ("C-k" . modi/ivy-kill-buffer))

    ;; Override the default binding for `upcase-word'
    (bind-key "M-u" #'ivy-resume modi-mode-map)
    (bind-key "M-o" #'ivy-recentf)))


(provide 'setup-ivy)

;; Call `ivy-immediate-done' if you want to use whatever you typed in the
;; search field, and ignore the suggestions provided by ivy in the list.
;;
;;  C-u <`ivy-alt-done' binding> <-- `ivy-immediate-done'
;;
;; This is useful especially when renaming files (and the name you want to
;; rename to partially matches one of the existing files).
;;
;; |----------------------------+-----------------------------------------------------|
;; | Command                    | Function                                            |
;; |----------------------------+-----------------------------------------------------|
;; | ivy-done                   | Exit the minibuffer with the selected candidate.    |
;; |                            | Try to leave `ivy' as soon as possible.             |
;; | ivy-alt-done               | Exit the minibuffer with the selected candidate.    |
;; |                            | When ARG is t, acts like `ivy-immediate-done'.      |
;; |                            | Try NOT to leave `ivy' at the soonest. For          |
;; |                            | instance, if a directory name completion is         |
;; |                            | possible, do that and list that directory's         |
;; |                            | content in `ivy' instead of opening that dir        |
;; |                            | in `dired'.                                         |
;; | ivy-immediate-done         | Exit the minibuffer with the current text,          |
;; |                            | ignoring the candidates.                            |
;; | ivy-call                   | Call the current action without exiting completion. |
;; | ivy-next-line-and-call     | Move cursor vertically down ARG candidates.         |
;; |                            | Call the permanent action if possible.              |
;; | ivy-previous-line-and-call | Move cursor vertically up ARG candidates.           |
;; |                            | Call the permanent action if possible.              |
;; |----------------------------+-----------------------------------------------------|
