;; Time-stamp: <2014-10-31 11:10:26 kmodi>

;; Neotree
;; https://github.com/chrisdone/chrisdone-emacs/blob/master/packages/neotree/neotree.el

(req-package neotree
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

    (defun modi/neotree-go-up-dir ()
      (interactive)
      (goto-char (point-min))
      (forward-line 2)
      (neotree-change-root))

    (bind-keys
     :map neotree-mode-map
     ("^"          . modi/neotree-go-up-dir)
     ("<C-return>" . neotree-change-root)
     ("C"          . neotree-change-root)
     ("c"          . neotree-create-node)
     ("+"          . neotree-create-node)
     ("d"          . neotree-delete-node)
     ("r"          . neotree-rename-node)
     ("e"          . neotree-enter))

    (bind-to-modi-map "n" neotree)))


(provide 'setup-neotree)
