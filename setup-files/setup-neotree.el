;; Time-stamp: <2015-10-05 09:19:08 kmodi>

;; Neotree
;; https://github.com/jaypei/emacs-neotree

(use-package neotree
  :init
  (progn
    (setq-default neo-smart-open t) ;  every time when the neotree window is
                                        ;  opened, it will try to find current
                                        ;  file and jump to node.
    (setq-default neo-dont-be-alone t) ; Don't allow neotree to be the only open
                                        ; window
    )
  :config
  (progn
    (setq neo-theme 'nerd) ; 'classic, 'nerd, 'ascii, 'arrow

    (setq neo-vc-integration '(face char))

    ;; Patch to fix vc integration
    (defun neo-vc-for-node (node)
      (let* ((backend (vc-backend node))
             (vc-state (when backend (vc-state node backend))))
        ;; (message "%s %s %s" node backend vc-state)
        (cons (cdr (assoc vc-state neo-vc-state-char-alist))
              (cl-case vc-state
                (up-to-date       neo-vc-up-to-date-face)
                (edited           neo-vc-edited-face)
                (needs-update     neo-vc-needs-update-face)
                (needs-merge      neo-vc-needs-merge-face)
                (unlocked-changes neo-vc-unlocked-changes-face)
                (added            neo-vc-added-face)
                (removed          neo-vc-removed-face)
                (conflict         neo-vc-conflict-face)
                (missing          neo-vc-missing-face)
                (ignored          neo-vc-ignored-face)
                (unregistered     neo-vc-unregistered-face)
                (user             neo-vc-user-face)
                (t                neo-vc-default-face)))))

    (defun modi/neotree-go-up-dir ()
      (interactive)
      (goto-char (point-min))
      (forward-line 2)
      (neotree-change-root))

    ;; http://emacs.stackexchange.com/a/12156/115
    (defun modi/find-file-next-in-dir (&optional prev)
      "Open the next file in the directory.
When PREV is non-nil, open the previous file in the directory."
      (interactive "P")
      (let ((neo-init-state (neo-global--window-exists-p)))
        (if (null neo-init-state)
            (neotree-show))
        (neo-global--select-window)
        (if (if prev
                (neotree-previous-line)
              (neotree-next-line))
            (progn
              (neo-buffer--execute nil
                                   (quote neo-open-file)
                                   (lambda (full-path &optional arg)
                                     (message "Reached dir: %s/" full-path)
                                     (if prev
                                         (neotree-next-line)
                                       (neotree-previous-line)))))
          (progn
            (if prev
                (message "You are already on the first file in the directory.")
              (message "You are already on the last file in the directory."))))
        (if (null neo-init-state)
            (neotree-hide))))

    (defun modi/find-file-prev-in-dir ()
      "Open the next file in the directory."
      (interactive)
      (modi/find-file-next-in-dir :prev))

    (bind-keys
     :map modi-mode-map
      ("C-c >" . modi/find-file-next-in-dir)
      ("C-c <" . modi/find-file-prev-in-dir))

    (bind-keys
     :map neotree-mode-map
      ("^"          . modi/neotree-go-up-dir)
      ("<C-return>" . neotree-change-root)
      ("C"          . neotree-change-root)
      ("c"          . neotree-create-node)
      ("+"          . neotree-create-node)
      ("d"          . neotree-delete-node)
      ("r"          . neotree-rename-node))))


(provide 'setup-neotree)
