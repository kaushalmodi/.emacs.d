;; Time-stamp: <2016-09-30 10:39:28 kmodi>

;; All things diff

;; Contents:
;;
;;  vc
;;  Diff-hl
;;  Ediff

;;; vc
(use-package vc
  :bind (:map modi-mode-map
         ("C-x v =" . modi/vc-diff)
         ("C-x v H" . vc-region-history)) ; New command in emacs 25.x
  :config
  (progn
    (defun modi/vc-diff (no-whitespace)
      "Call `vc-diff' as usual.
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
        (call-interactively #'vc-diff)))))

;;; Diff-hl
;; https://github.com/dgutov/diff-hl
(use-package diff-hl
  :bind (:map modi-mode-map
         ("s-v" . hydra-diff-hl/body)
         ("C-c v" . hydra-diff-hl/body))
  :init
  (progn
    (defvar modi/diff-hl-mode-hooks '(emacs-lisp-mode-hook
                                      conf-space-mode-hook ; .tmux.conf
                                      sh-mode-hook)
      "List of hooks of major modes in which diff-hl-mode should be enabled.")

    (dolist (hook modi/diff-hl-mode-hooks)
      (add-hook hook #'diff-hl-mode))

    (defhydra hydra-diff-hl (:color red)
      "diff-hl"
      ("="     diff-hl-diff-goto-hunk "goto hunk")
      ("<RET>" diff-hl-diff-goto-hunk "goto hunk")
      ("u"     diff-hl-revert-hunk    "revert hunk")
      ("["     diff-hl-previous-hunk  "prev hunk")
      ("p"     diff-hl-previous-hunk  "prev hunk")
      ("]"     diff-hl-next-hunk      "next hunk")
      ("n"     diff-hl-next-hunk      "next hunk")
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


(provide 'setup-diff)
