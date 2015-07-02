;; Time-stamp: <2015-07-02 11:21:44 kmodi>

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
^^_,_      _f_ollow  _i_nsert  ^^_c_alling %s(if ivy-calling \"on\" \"off\")
_p_/_n_    _d_one    _q_uit    ^^_m_atcher %s(if (eq ivy--regex-function 'ivy--regex-fuzzy) \"fuzzy\" \"ivy\")
^^_._      _D_o it!  ^^        _<_/_>_ shrink/grow window
"
          ;; arrows
          (","   ivy-beginning-of-buffer)
          ("p"   ivy-previous-line)
          ("n"   ivy-next-line)
          ("."   ivy-end-of-buffer)
          ;; actions
          ("f"   ivy-alt-done         :exit nil)
          ("C-m" ivy-alt-done         :exit nil) ; RET
          ("d"   ivy-done             :exit t)
          ("C-j" ivy-done             :exit t)
          ("D"   ivy-immediate-done   :exit t)
          ("c"   ivy-toggle-calling)
          ("m"   ivy-toggle-fuzzy)
          (">"   ivy-minibuffer-grow)
          ("<"   ivy-minibuffer-shrink)
          ;; quit hydra
          ("i"   nil)
          ("C-o" nil)
          ;; quit ivy
          ("q"   keyboard-escape-quit :exit t)
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

    (bind-keys
     :map modi-mode-map
      ;; Override the default binding for `upcase-word'
      ("M-u"     . ivy-resume)
      ;; Override the default binding for `delete-blank-lines'
      ("C-x C-o" . ivy-recentf))))


(provide 'setup-ivy)

;; Call `ivy-immediate-done' if you want to use whatever you typed in the
;; search field, and ignore the suggestions provided by ivy in the list.
;;
;;  C-u <`ivy-alt-done' binding> <-- `ivy-immediate-done'
;;
;; This is useful especially when renaming files (and the name you want to
;; rename to partially matches one of the existing files).
