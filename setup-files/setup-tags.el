;; Time-stamp: <2015-03-11 14:46:34 kmodi>

;;;; ctags
;; https://github.com/fishman/ctags
;; Use Exuberant ctags from github instead of the ctags that comes with emacs.

;; Don't ask before rereading the TAGS files if they have changed
(setq tags-revert-without-query t)
(setq tags-case-fold-search nil) ; t=case-insensitive, nil=case-sensitive
;; Increase the warning threshold to be more than normal TAGS file sizes
(setq large-file-warning-threshold 50000000) ; 50MB

(when (executable-find "ctags")
;;; etags-table
  ;; Depending on the location of the file in buffer, the respective TAGS file is
  ;; opened on doing a tag find.
  (use-package etags-table
      :ensure t
      :config
      (progn
        (setq etags-table-alist nil) ; initialize `etags-table-alist'

        ;; emacs config
        (add-to-list 'etags-table-alist
                     `(,(concat user-emacs-directory "/.*")
                        ,(concat user-emacs-directory "/TAGS")))

        ;; add-to-list if uvm-source-code-dir symbol is defined
        (when (boundp 'uvm-source-code-dir)
          (add-to-list 'etags-table-alist
                       `(,(concat uvm-source-code-dir "/.*")
                          ,(concat uvm-source-code-dir "/TAGS"))
                       t))
        ;; Max depth to search up for a tags file; nil means don't search
        (setq etags-table-search-up-depth 15)

;;; etags-select
        ;; http://mattbriggs.net/blog/2012/03/18/awesome-emacs-plugins-ctags
        (use-package etags-select
            :ensure t
            :config
            (progn
              (bind-keys
               :map etags-select-mode-map
               ("C-g" . etags-select-quit)
               ("C-p" . etags-select-previous-tag)
               ("C-n" . etags-select-next-tag))
              ;; default etags-select bindings
              ;;  Return -> 'etags-select-goto-tag
              ;;  M-Return -> 'etags-select-goto-tag-other-window
              ;;  p -> 'etags-select-previous-tag
              ;;  n -> 'etags-select-next-tag
              ;;  q -> 'etags-select-quit
              ;;  0 -> (etags-select-by-tag-number "0")
              ;;  1 -> (etags-select-by-tag-number "1")
              ;;  ..                               ..
              ;;  9 -> (etags-select-by-tag-number "9")

              ;; Below function comes useful when you change the project-root
              ;; symbol to a different value (when switching projects)
              (defun update-etags-table-then-find-tag ()
                "Update etags-table based on the current project directory."
                (interactive)
                (when (featurep 'projectile)
                  (add-to-list 'etags-table-alist
                               `(,(concat (projectile-project-root) ".*")
                                  ,(concat (projectile-project-root) "TAGS"))
                               t))
                (etags-select-find-tag-at-point))))))

;;; ctags-update
  ;; https://github.com/jixiuf/helm-etags-plus
  (use-package ctags-update
      :ensure t
      :commands (turn-on-ctags-auto-update-mode ctags-update)
      :config
      (progn
        ;; Auto update
        (setq ctags-update-delay-seconds (* 30 60)) ; every 1/2 hour
        (add-hook 'verilog-mode-hook    #'turn-on-ctags-auto-update-mode)
        (add-hook 'emacs-lisp-mode-hook #'turn-on-ctags-auto-update-mode))))
;; `ctags-update'
;;   update TAGS in parent directory using `exuberant-ctags'.
;; `ctags-auto-update-mode'
;;   auto update TAGS using `exuberant-ctags' in parent directory.
;; `turn-on-ctags-auto-update-mode'
;;   turn on `ctags-auto-update-mode'.


;;;; gtags, GNU global

(when (executable-find "global")
  (use-package ggtags
      :ensure t
      :config
      (progn

        (defun my/ggtags-project-name ()
          "Return gtags project name."
          (if (stringp ggtags-project-root)
              (let ((project-root ggtags-project-root)
                    project-name)
                (setq project-name (file-name-nondirectory
                                    (directory-file-name project-root)))
                (while (string-match "\\(sos_\\|\\bsrc\\b\\)" project-name)
                  (setq project-root (replace-regexp-in-string
                                      "\\(.*\\)/.+/*$" "\\1" project-root))
                  (setq project-name (file-name-nondirectory
                                      (directory-file-name project-root))))
                project-name)
            nil))

        ;; (setq ggtags-navigation-mode-lighter nil)
        ;; (setq ggtags-mode-line-project-name nil)
        (setq ggtags-mode-line-project-name
              '("[" (:eval (let ((name (if (stringp (my/ggtags-project-name))
                                           (my/ggtags-project-name)
                                         "?")))
                             (propertize
                              name 'face compilation-info-face
                              'help-echo (if (stringp ggtags-project-root)
                                             (concat "mouse-1 to visit " ggtags-project-root)
                                           "mouse-1 to set project")
                              'mouse-face 'mode-line-highlight
                              'keymap ggtags-mode-line-project-keymap)))
                "]"))

        (setq ggtags-oversize-limit (* 30 1024 1024)) ; 30 MB

        (dolist (hook '(verilog-mode-hook
                        matlab-mode-hook
                        sh-mode-hook
                        cperl-mode-hook
                        c-mode-hook))
          (add-hook hook #'ggtags-mode))

        ;; Patch - Enable "-d" option
        (defun ggtags-global-build-command (cmd &rest args)
          ;; CMD can be definition, reference, symbol, grep, idutils
          (let ((xs (append (list (shell-quote-argument (ggtags-program-path "global"))
                                  "-v"
                                  (format "--result=%s" ggtags-global-output-format)
                                  (and ggtags-global-ignore-case "--ignore-case")
                                  (and ggtags-global-use-color
                                       (ggtags-find-project)
                                       (ggtags-project-has-color (ggtags-find-project))
                                       "--color=always")
                                  (and (ggtags-find-project)
                                       (ggtags-project-has-path-style (ggtags-find-project))
                                       "--path-style=shorter")
                                  (and ggtags-global-treat-text "--other")
                                  (pcase cmd
                                    ((pred stringp) cmd)
                                    (`definition "-d") ;-d not supported by Global 5.7.1
                                    (`reference "--reference")
                                    (`symbol "--symbol")
                                    (`path "--path")
                                    (`grep "--grep")
                                    (`idutils "--idutils")))
                            args)))
            (mapconcat #'identity (delq nil xs) " ")))

        ;; Don't consider ` (back quote) as part of `tag' when looking for a
        ;; Verilog macro definition
        (defun ggtags-tag-at-point ()
          (pcase (funcall ggtags-bounds-of-tag-function)
            (`(,beg . ,end)
              (if (eq ?` (string-to-char (buffer-substring beg end)))
                  ;; If `(buffer-substring beg end)' returns "`uvm_info" (for example),
                  ;; discard the ` and return just "uvm_info"
                  (buffer-substring (1+ beg) end)
                ;; else return the whole `(buffer-substring beg end)'
                (buffer-substring beg end)))))

        ;; Remove the default binding for `M-.' in `ggtags-mode-map'
        (define-key ggtags-mode-map (kbd "M-.") nil)

        (when (featurep 'key-chord)
          (key-chord-define-global "??" #'ggtags-show-definition)))))

(defun modi/find-tag (&optional arg)
  "Use ctags by default and gtags when called with `C-u' prefix."
  (interactive "P")
  (if (or (null arg)
          (null ggtags-mode))
      (update-etags-table-then-find-tag)
    (ggtags-find-definition (ggtags-tag-at-point))))
(bind-key "M-." #'modi/find-tag)


(provide 'setup-tags)

;; Emacs rereads the TAGS file (ctags) during every tag find operation.
