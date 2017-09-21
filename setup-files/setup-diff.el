;; Time-stamp: <2017-09-21 16:52:09 kmodi>

;; All things diff

;; Contents:
;;
;;  add-log
;;  vc-git
;;    vc
;;  Diff-hl
;;  Ediff
;;  Smerge

;;; add-log
(use-package add-log
  :commands (modi/add-change-log-entry-other-window-and-return)
  :config
  (progn
    (defun modi/add-change-log-entry-other-window-and-return ()
      "Call `add-change-log-entry-other-window' and return to the previous window."
      (interactive)
      (add-change-log-entry-other-window)
      (select-window (previous-window)))))

;;; vc-git
(use-package vc-git
  ;; Auto-load `vc-git' when `vc-git-root' is used (in /setup-verilog.el)
  :commands (vc-git-root))

;;;; vc
(use-package vc
  :bind (:map modi-mode-map
         ("C-x v =" . modi/vc-diff)
         ("C-x v H" . vc-region-history)) ; New command in emacs 25.x
  :config
  (progn
    (defun modi/vc-diff (no-whitespace)
      "Call `vc-diff' as usual if buffer is not modified.
If the buffer is modified (yet to be saved), call `diff-buffer-with-file'.

If NO-WHITESPACE is non-nil, ignore all white space when doing diff."
      (interactive "P")
      (let* ((no-ws-switch '("-w"))
             (vc-git-diff-switches (if no-whitespace
                                       no-ws-switch
                                     vc-git-diff-switches))
             (vc-diff-switches (if no-whitespace
                                   no-ws-switch
                                 vc-diff-switches))
             (diff-switches (if no-whitespace
                                no-ws-switch
                              diff-switches))
             ;; Set `current-prefix-arg' to nil so that the HISTORIC arg
             ;; of `vc-diff' stays nil.
             current-prefix-arg)
        (if (buffer-modified-p)
            (diff-buffer-with-file (current-buffer))
          (call-interactively #'vc-diff))))))

;;; Diff-hl
;; https://github.com/dgutov/diff-hl
(use-package diff-hl
  :bind (:map modi-mode-map
         ("s-v" . hydra-diff-hl/body)
         ("C-c v" . hydra-diff-hl/body))
  :init
  (progn
    (defconst modi/diff-hl-mode-hooks '(emacs-lisp-mode-hook
                                        conf-space-mode-hook ;.tmux.conf
                                        markdown-mode-hook
                                        css-mode-hook
                                        web-mode-hook
                                        sh-mode-hook
                                        yaml-mode-hook ;tmuxp yaml configs
                                        c-mode-hook
                                        makefile-mode-hook
                                        org-mode-hook)
      "List of hooks of major modes in which diff-hl-mode should be enabled.")

    (dolist (hook modi/diff-hl-mode-hooks)
      (add-hook hook #'diff-hl-mode))

    (defhydra hydra-diff-hl (:color red)
      "diff-hl"
      ("=" diff-hl-diff-goto-hunk "goto hunk")
      ("<RET>" diff-hl-diff-goto-hunk "goto hunk")
      ("u" diff-hl-revert-hunk "revert hunk")
      ("[" diff-hl-previous-hunk "prev hunk")
      ("p" diff-hl-previous-hunk "prev hunk")
      ("]" diff-hl-next-hunk "next hunk")
      ("n" diff-hl-next-hunk "next hunk")
      ("a" modi/add-change-log-entry-other-window-and-return "add change log entry")
      ("q" nil "cancel"))

    (add-hook 'dired-mode-hook #'diff-hl-dired-mode)))

;;; Ediff
(use-package ediff
  :commands (modi/ediff-dwim)
  :config
  (progn
    ;; No separate frame for ediff control buffer
    (setq ediff-window-setup-function #'ediff-setup-windows-plain)

    ;; Split windows horizontally in ediff (instead of vertically)
    (setq ediff-split-window-function #'split-window-horizontally)

    (defun modi/ediff-dwim ()
      "Do ediff as I mean.

If a region is active, call `ediff-regions-wordwise'.
Else if the frame has 2 windows with identical major modes,
  - Do `ediff-files' if the buffers are associated to files and the buffers
    have not been modified.
  - Do `ediff-buffers' otherwise.
Else if the current is a file buffer with a VC backend, call `vc-ediff'
Else call `ediff-buffers'."
      (interactive)
      (let* ((num-win (safe-length (window-list)))
             (bufa (get-buffer (buffer-name)))
             (filea (buffer-file-name bufa))
             (modea (with-current-buffer bufa major-mode))
             bufb fileb modeb)
        (save-excursion
          (other-window 1)
          (setq bufb (get-buffer (buffer-name)))
          (setq fileb (buffer-file-name bufb))
          (setq modeb (with-current-buffer bufb major-mode)))
        (cond
         ;; If a region is selected
         ((region-active-p)
          (call-interactively #'ediff-regions-wordwise))
         ;; Else if 2 windows with same major modes
         ((and (= 2 num-win)
               (eq modea modeb))
          (if ;; If either of the buffers is not associated to a file,
              ;; or if either of the buffers is modified
              (or (null filea)
                  (null fileb)
                  (buffer-modified-p bufa)
                  (buffer-modified-p bufb))
              (progn
                (message "Running (ediff-buffers \"%s\" \"%s\") .." bufa bufb)
                (ediff-buffers bufa bufb))
            (progn
              (message "Running (ediff-files \"%s\" \"%s\") .." filea fileb)
              (ediff-files filea fileb))))
         ;; Else if file in current buffer has a vc backend
         ((and filea
               (vc-registered filea))
          (call-interactively #'vc-ediff))
         ;; Else call `ediff-buffers'
         (t
          (call-interactively #'ediff-buffers)))))))

;;; Smerge
(use-package smerge-mode
  :bind (:map modi-mode-map
         ("C-c m" . hydra-smerge/body))
  :init
  (progn
    (defun modi/enable-smerge-maybe ()
      "Auto-enable `smerge-mode' when merge conflict is detected."
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^<<<<<<< " nil :noerror)
          (smerge-mode 1))))
    (add-hook 'find-file-hook #'modi/enable-smerge-maybe :append))
  :config
  (progn
    (>=e "26.0"
        nil
      ;; http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=bdfee01a6567b9f08f82bc84d1196e6cb62587ca
      (defalias 'smerge-keep-upper 'smerge-keep-mine)
      (defalias 'smerge-keep-lower 'smerge-keep-other)
      (defalias 'smerge-diff-base-upper 'smerge-diff-base-mine)
      (defalias 'smerge-diff-upper-lower 'smerge-diff-mine-other)
      (defalias 'smerge-diff-base-lower 'smerge-diff-base-other))

    (defhydra hydra-smerge (:color pink
                            :hint nil
                            :pre (smerge-mode 1)
                            ;; Disable `smerge-mode' when quitting hydra if
                            ;; no merge conflicts remain.
                            :post (smerge-auto-leave))
      "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
      ("n" smerge-next)
      ("p" smerge-prev)
      ("b" smerge-keep-base)
      ("u" smerge-keep-upper)
      ("l" smerge-keep-lower)
      ("a" smerge-keep-all)
      ("RET" smerge-keep-current)
      ("\C-m" smerge-keep-current)
      ("<" smerge-diff-base-upper)
      ("=" smerge-diff-upper-lower)
      (">" smerge-diff-base-lower)
      ("R" smerge-refine)
      ("E" smerge-ediff)
      ("C" smerge-combine-with-next)
      ("r" smerge-resolve)
      ("k" smerge-kill-current)
      ("q" nil "cancel" :color blue))))


(provide 'setup-diff)
