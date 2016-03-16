;; Time-stamp: <2016-03-16 10:07:15 kmodi>

;; Ivy (comes packaged with the `swiper' package)

(use-package ivy
  :config
  (progn
    ;; Disable ido
    (with-eval-after-load 'ido
      (ido-mode -1)
      ;; Enable ivy
      (ivy-mode 1))

    ;; Show recently killed buffers when calling `ivy-switch-buffer'
    (setq ivy-use-virtual-buffers t)
    (setq ivy-virtual-abbreviate 'full) ; Show the full virtual file paths

    (setq ivy-count-format "%d/%d ")
    (setq ivy-re-builders-alist '((t . ivy--regex-plus))) ; default
    ;; (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))

    (use-package ivy-hydra
      :config
      (progn
        (defhydra hydra-ivy (:hint nil
                             :color pink)
          "
^^_,_        _f_ollow      occ_u_r      _g_o          ^^_c_alling %-7s(if ivy-calling \"on\" \"off\")      _w_/_s_/_a_: %-14s(ivy-action-name)
_p_/_n_      _d_one        ^^           _i_nsert      ^^_m_atcher %-7s(ivy--matcher-desc)^^^^^^^^^^^^      _C_ase-fold: %-10`ivy-case-fold-search
^^_._        _D_o it!      ^^           _q_uit        _<_/_>_ shrink/grow^^^^^^^^^^^^^^^^^^^^^^^^^^^^      _t_runcate: %-11`truncate-lines
"
          ;; arrows
          (","   ivy-beginning-of-buffer) ; default h
          ("p"   ivy-previous-line) ; default j
          ("n"   ivy-next-line) ; default k
          ("."   ivy-end-of-buffer) ; default l
          ;; actions
          ("f"   ivy-alt-done         :exit nil)
          ("C-m" ivy-alt-done         :exit nil) ; RET, default C-j
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
          ("a"   ivy-read-action)
          ("t"   (setq truncate-lines (not truncate-lines)))
          ("C"   ivy-toggle-case-fold)
          ("u"   ivy-occur :exit t)
          ;; quit hydra
          ("i"   nil)
          ("C-o" nil)
          ;; quit ivy
          ("q"   keyboard-escape-quit :exit t); default o
          ("C-g" keyboard-escape-quit :exit t))))

    ;; Exchange the default bindings for C-j and C-m
    (bind-keys
     :map ivy-minibuffer-map
      ("C-m"   . ivy-alt-done) ; RET
      ("C-j"   . ivy-done)
      ("C-S-m" . ivy-immediate-done)
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
