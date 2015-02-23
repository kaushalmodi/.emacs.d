;; Time-stamp: <2015-02-23 11:42:33 kmodi>

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
    ;; https://github.com/jaypei/emacs-neotree/issues/58
    ;; FIX 1
    ;; `neo-persist-show' has to be set to `nil' to prevent the "Attempt to
    ;; delete minibuffer or sole ordinary window." error when hitting `TAB'
    ;; in the minibuffer. TAB > ido-complete > ido-completion-help > popwin >
    ;; delete-other-windows > activates the defadvice set in neotree.el.
    ;; Setting below var to nil doesn't activate that defadvice.
    ;; (setq-default neo-persist-show nil)
    ;; FIX 2
    ;; Fix from author: http://www.emacswiki.org/emacs/NeoTree (title Popwin)
    (setq-default neo-persist-show t)
    (when neo-persist-show
      (add-hook 'popwin:before-popup-hook
                (lambda () (setq neo-persist-show nil)))
      (add-hook 'popwin:after-popup-hook
                (lambda () (setq neo-persist-show t))))

    (setq neo-theme 'nerd) ; 'classic, 'nerd, 'ascii, 'arrow

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

    (bind-to-modi-map "n" neotree-toggle)))


(provide 'setup-neotree)
