;; Time-stamp: <2015-11-04 11:44:18 kmodi>

;; Setup for different tags

;; Contents:
;;
;;  gtags, GNU global
;;    ggtags
;;  ctags
;;    etags-select
;;      etags-table
;;    ctags-update
;;  modi/find-tag

;;; gtags, GNU global

(when (executable-find "global")
;;;; ggtags
  ;; https://github.com/leoliu/ggtags
  (use-package ggtags
    :config
    (progn
      (setq ggtags-sort-by-nearness t) ; Requires global 6.5+

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

      (setq ggtags-navigation-mode-lighter nil)
      (setq ggtags-mode-line-project-name
            '("[" (:eval (let ((name (if (stringp (my/ggtags-project-name))
                                         (my/ggtags-project-name)
                                       "?")))
                           (propertize name
                                       'face font-lock-type-face
                                       'help-echo (if (stringp ggtags-project-root)
                                                      (concat "mouse-1 to visit "
                                                              ggtags-project-root)
                                                    "mouse-1 to set project")
                                       'mouse-face 'mode-line-highlight
                                       'keymap ggtags-mode-line-project-keymap)))
              "]"))

      (setq ggtags-oversize-limit (* 30 1024 1024)) ; 30 MB

      (dolist (hook '(verilog-mode-hook
                      matlab-mode-hook
                      sh-mode-hook
                      cperl-mode-hook
                      c-mode-hook
                      makefile-mode-hook
                      conf-space-mode-hook))
        (add-hook hook #'ggtags-mode))

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
      (bind-key "M-." nil ggtags-mode-map)

      (key-chord-define-global "??" #'ggtags-show-definition))))

;;; ctags
;; https://github.com/universal-ctags/ctags
;; Use Universal (earlier called Exuberant) ctags from github instead of the
;; ctags that comes with emacs.

;; Don't ask before rereading the TAGS files if they have changed
(setq tags-revert-without-query t)
(setq tags-case-fold-search nil) ; t=case-insensitive, nil=case-sensitive

;; Increase the warning threshold to be more than normal TAGS file sizes
(setq large-file-warning-threshold (* 50 1024 1024)) ; 50MB

(when (executable-find "ctags")
;;;; etags-select
  ;; http://mattbriggs.net/blog/2012/03/18/awesome-emacs-plugins-ctags
  (use-package etags-select
    :commands (modi/update-etags-table)
    :config
    (progn

;;;;; etags-table
      ;; Depending on the location of the file in buffer, the respective TAGS
      ;; file is opened on doing a tag find.
      (use-package etags-table
        :config
        (progn
          (setq etags-table-alist nil) ; initialize `etags-table-alist'

          ;; emacs config
          (add-to-list 'etags-table-alist
                       `(,(concat user-emacs-directory ".*")
                         ,(concat user-emacs-directory "TAGS")))

          ;; Max depth to search up for a tags file; nil means don't search
          (setq etags-table-search-up-depth 15)))

      ;; Below function comes useful when you change the project-root
      ;; symbol to a different value (when switching projects)
      (defun modi/update-etags-table ()
        "Update `etags-table-alist' based on the current project directory."
        (interactive)
        (when (featurep 'projectile)
          (add-to-list 'etags-table-alist
                       `(,(concat (projectile-project-root) ".*")
                         ,(concat (projectile-project-root) "TAGS"))
                       t)))

      (bind-keys
       :map etags-select-mode-map
        ("C-g" . etags-select-quit))))

;;;; ctags-update
  ;; https://github.com/jixiuf/helm-etags-plus
  (use-package ctags-update
    :config
    (progn
      ;; Auto update
      (setq ctags-update-delay-seconds (* 30 60)) ; every 1/2 hour
      (add-hook 'verilog-mode-hook    #'turn-on-ctags-auto-update-mode)
      (add-hook 'emacs-lisp-mode-hook #'turn-on-ctags-auto-update-mode))))

;;; modi/find-tag

(defun modi/find-tag (&optional use-ctags)
  "Use `ggtags' if available, else use `ctags' to find tags.

If USE-CTAGS is non-nil, use `ctags'."
  (interactive "P")
  (if (or use-ctags
          (not (featurep 'ggtags)))
      (progn
        (modi/update-etags-table)
        (etags-select-find-tag-at-point))
    (call-interactively #'ggtags-find-tag-dwim)))
(bind-key "M-." #'modi/find-tag)


(provide 'setup-tags)

;; Emacs rereads the TAGS file (ctags) during every tag find operation.

;; Default `etags-select' bindings
;; |---------+------------------------------------|
;; | Binding | Description                        |
;; |---------+------------------------------------|
;; | RET     | etags-select-goto-tag              |
;; | M-RET   | etags-select-goto-tag-other-window |
;; | p       | etags-select-previous-tag          |
;; | n       | etags-select-next-tag              |
;; | q       | etags-select-quit                  |
;; | 0       | (etags-select-by-tag-number "0")   |
;; | 1       | (etags-select-by-tag-number "1")   |
;; | ..      | ..                                 |
;; | 9       | (etags-select-by-tag-number "9")   |
;; |---------+------------------------------------|
