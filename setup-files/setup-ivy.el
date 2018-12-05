;; Time-stamp: <2018-12-05 12:06:58 kmodi>

;; Ivy (better than ido in my opinion)

(use-package ivy
  :bind (:map modi-mode-map
         ("M-u" . ivy-resume)    ;Override the default binding for `upcase-word'
         ("C-c w" . ivy-push-view) ;Push window configuration to `ivy-views'
         ("C-c W" . ivy-pop-view)) ;Remove window configuration from `ivy-views'
  :config
  (progn
    ;; Disable ido
    (with-eval-after-load 'ido
      (ido-mode -1)
      ;; Enable ivy
      (ivy-mode 1))

    ;; Show recently killed buffers when calling `ivy-switch-buffer'
    (setq ivy-use-virtual-buffers t)
    (setq ivy-virtual-abbreviate 'full) ;Show the full virtual file paths

    (setq ivy-count-format "%d/%d ")
    (setq ivy-re-builders-alist '((t . ivy--regex-plus))) ;Default
    ;; (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))

    ;; Do not show "./" and "../" in the `counsel-find-file' completion list
    (setq ivy-extra-directories nil)    ;Default value: ("../" "./")

    ;; https://github.com/abo-abo/swiper/blob/master/ivy-hydra.el
    (use-package ivy-hydra
      :ensure t
      :config
      (progn
        ;; Re-define the `hydra-ivy' defined in `ivy-hydra' package.
        (defhydra hydra-ivy (:hint nil
                             :color pink)
          "
^ _,_ ^      _f_ollow      occ_u_r      _g_o          ^^_c_alling: %-7s(if ivy-calling \"on\" \"off\")      _w_(prev)/_s_(next)/_a_(read) action: %-14s(ivy-action-name)
_p_/_n_      _d_one        ^^           _i_nsert      ^^_m_atcher: %-7s(ivy--matcher-desc)^^^^^^^^^^^^      _C_ase-fold: %-10`ivy-case-fold-search
^ _._ ^      _D_o it!      ^^           _q_uit        _<_/_>_ shrink/grow^^^^^^^^^^^^^^^^^^^^^^^^^^^^       _t_runcate: %-11`truncate-lines
"
          ;; Arrows
          ("," ivy-beginning-of-buffer)      ;Default h
          ("p" ivy-previous-line)            ;Default j
          ("n" ivy-next-line)                ;Default k
          ("." ivy-end-of-buffer)            ;Default l
          ;; Quit ivy
          ("q" keyboard-escape-quit :exit t) ;Default o
          ("C-g" keyboard-escape-quit :exit t)
          ;; Quit hydra
          ("i" nil)
          ("C-o" nil)
          ;; actions
          ("f" ivy-alt-done :exit nil)
          ;; Exchange the default bindings for C-j and C-m
          ("C-m" ivy-alt-done :exit nil) ;RET, default C-j
          ("C-j" ivy-done :exit t)       ;Default C-m
          ("d" ivy-done :exit t)
          ("D" ivy-immediate-done :exit t)
          ("g" ivy-call)
          ("c" ivy-toggle-calling)
          ("m" ivy-rotate-preferred-builders)
          (">" ivy-minibuffer-grow)
          ("<" ivy-minibuffer-shrink)
          ("w" ivy-prev-action)
          ("s" ivy-next-action)
          ("a" ivy-read-action)
          ("t" (setq truncate-lines (not truncate-lines)))
          ("C" ivy-toggle-case-fold)
          ("u" ivy-occur :exit t)
          ("?" (ivy-exit-with-action    ;Default D
                (lambda (_) (find-function #'hydra-ivy/body))) "Definition of this hydra" :exit t))

        (bind-keys
         :map ivy-minibuffer-map
         ("C-t" . ivy-rotate-preferred-builders)
         ("C-o" . hydra-ivy/body)
         ("M-o" . ivy-dispatching-done-hydra))))

    (bind-keys
     :map ivy-minibuffer-map
     ;; Exchange the default bindings for C-j and C-m
     ("C-m" . ivy-alt-done)             ;RET, default C-j
     ("C-j" . ivy-done)                 ;Default C-m
     ("C-S-m" . ivy-immediate-done))

    (bind-keys
     :map ivy-occur-mode-map
     ("n" . ivy-occur-next-line)
     ("p" . ivy-occur-previous-line)
     ("b" . backward-char)
     ("f" . forward-char)
     ("v" . ivy-occur-press)            ;Default f
     ("RET" . ivy-occur-press))

    (with-eval-after-load 'setup-windows-buffers
      (bind-keys
       :map ivy-minibuffer-map
       ("C-x k" . modi/kill-buffer-dwim) ;Aborts recursive edit
       ("C-)" . modi/kill-buffer-dwim))) ;Aborts recursive edit

    (key-chord-define ivy-minibuffer-map "m," #'ivy-beginning-of-buffer)
    (key-chord-define ivy-minibuffer-map ",." #'ivy-end-of-buffer)

    ;; Bind C-k to kill a buffer directly from the list shown on doing M-x ivy-switch-buffer.
    ;; https://github.com/abo-abo/swiper/issues/164
    (defun modi/ivy-kill-buffer ()
      (interactive)
      (ivy-set-action 'kill-buffer)
      (ivy-done))
    (bind-keys
     :map ivy-switch-buffer-map
     ("C-k" . modi/ivy-kill-buffer))))

;; https://github.com/Yevgnen/ivy-rich
;; Richer "C-x b" buffer-switching Ivy interface.
(use-package ivy-rich
  :after ivy
  :ensure t
  :config
  (progn
    (ivy-rich-mode)))


(provide 'setup-ivy)

;; Call `ivy-immediate-done' if you want to use whatever you typed in the
;; search field, and ignore the suggestions provided by ivy in the list.
;;
;;  C-u <`ivy-alt-done' binding> <-- `ivy-immediate-done'
;;
;; This is useful especially when renaming files (and the name you want to
;; rename to partially matches one of the existing files).
;;
;; |----------------------------+----------------+------------------------------------------------------|
;; | Command                    | ivy map        | Function                                             |
;; |                            | Bindings       |                                                      |
;; |----------------------------+----------------+------------------------------------------------------|
;; | ivy-done                   | C-j            | Exit the minibuffer with the selected candidate.     |
;; |                            | (default: C-m) | Try to leave `ivy' as soon as possible.              |
;; |----------------------------+----------------+------------------------------------------------------|
;; | ivy-alt-done               | C-m or RET     | Exit the minibuffer with the selected candidate.     |
;; |                            | (default: C-j) | When ARG is t, acts like `ivy-immediate-done'.       |
;; |                            |                | Try NOT to leave `ivy' at the soonest. For           |
;; |                            |                | instance, if a directory name completion is          |
;; |                            |                | possible, do that and list that directory's          |
;; |                            |                | content in `ivy' instead of opening that dir         |
;; |                            |                | in `dired'.                                          |
;; |----------------------------+----------------+------------------------------------------------------|
;; | ivy-immediate-done         | C-S-m          | Exit the minibuffer with the current text,           |
;; |                            |                | ignoring the candidates.                             |
;; |----------------------------+----------------+------------------------------------------------------|
;; | ivy-partial-or-done        | TAB            | Attempts partial completion, extending current line  |
;; |                            |                | input as much as possible. "TAB TAB" is the same as  |
;; |                            |                | `ivy-alt-done'.                                      |
;; |----------------------------+----------------+------------------------------------------------------|
;; | ivy-call                   | C-M-m          | Call the current action without exiting completion.  |
;; |----------------------------+----------------+------------------------------------------------------|
;; | ivy-next-line-and-call     | C-M-n          | Move cursor vertically down ARG candidates.          |
;; |                            |                | Call the permanent action if possible.               |
;; | ivy-previous-line-and-call | C-M-p          | Move cursor vertically up ARG candidates.            |
;; |                            |                | Call the permanent action if possible.               |
;; |----------------------------+----------------+------------------------------------------------------|
;; | ivy-dispatching-done       | M-o            | Presents valid actions from which to choose. When    |
;; |                            |                | only one action is available, there is no difference |
;; |                            |                | between this and `ivy-done'.                         |
;; |----------------------------+----------------+------------------------------------------------------|

;; Switch to any of the saved `ivy-views' using `M-x ivy-switch-buffer'.
;; When `ivy-mode' is enabled, binding for `switch-to-buffer' is remapped to
;; `ivy-switch-buffer'.
