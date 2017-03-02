;; Time-stamp: <2017-03-02 18:44:08 kmodi>

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
;;  xref, semantic/symref

;;; gtags, GNU global

(when (executable-find "global")
;;;; ggtags
  ;; https://github.com/leoliu/ggtags
  (use-package ggtags
    :config
    (progn
      (setq ggtags-sort-by-nearness nil) ; Enabling nearness requires global 6.5+
      (setq ggtags-navigation-mode-lighter nil)
      (setq ggtags-mode-line-project-name nil)
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
      ;; Remove the default binding for `M-o' in `ggtags-navigation-map'
      (bind-key "M-o" nil ggtags-navigation-map)

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
        (when (and (featurep 'projectile)
                   (projectile-project-root))
          (add-to-list 'etags-table-alist
                       `(,(concat (projectile-project-root) ".*")
                         ,(concat (projectile-project-root) "TAGS"))
                       t)))

      (bind-keys
       :map etags-select-mode-map
        ("C-g" . etags-select-quit))))

;;;; ctags-update
  ;; https://github.com/jixiuf/ctags-update
  (use-package ctags-update
    :config
    (progn
      ;; Auto update
      (setq ctags-update-delay-seconds (* 30 60)) ; every 1/2 hour

      ;; Override `ctags-update-how-to-update' so that when it called
      ;; non-interactively (via `after-save-hook), then the user is not nagged
      ;; to generate the TAGS file if it is not present. If TAGS file generation
      ;; is necessary, do M-x ctags-update.
      (defun modi/ctags-update-how-to-update (is-interactive)
        "Return the TAGS file name (maybe).

If \\[universal-argument] or \\[universal-argument] \\[universal-argument]
argument is used when calling `ctags-update' interactively, user is asked for
the location to generate the TAGS file.

If `ctags-update' is called interactively without any prefix argument, user is
asked for the TAGS file location, but only if that file is not present.

Otherwise, if `ctags-update' is called non-interactively (example, via the
`after-save-hook'), if the TAGS file is present, return that file's path; else
do nothing and return nil.

This function also prevents the user-error \"Another ctags-update process is
already running\" caused in `ctags-update' function if value returned by this
function is non-nil and the tag generation process is already running."
        (let (tags)
          (cond
           ((> (prefix-numeric-value current-prefix-arg) 1)  ;C-u or C-uC-u ,generate new tags in selected directory
            (setq tags (expand-file-name "TAGS"
                                         (read-directory-name "Generate TAGS in dir:"))))
           (is-interactive
            (setq tags (ctags-update-find-tags-file))
            (unless tags
              (setq tags (expand-file-name "TAGS"
                                           (read-directory-name "Generate TAGS in dir:")))))
           (t
            ;; If the TAGS file does not exist in this case, `tags' is set to nil.
            (setq tags (ctags-update-find-tags-file))
            (when tags
              (let ((process-already-running (get-process tags)))
                (when process-already-running
                  ;; Prevent the user-error "Another ctags-update process is
                  ;; already running" caused in `ctags-update' function if tags
                  ;; is non-nil and the tag generation process is already running.
                  (setq tags nil))))))
          tags))
      (advice-add 'ctags-update-how-to-update :override #'modi/ctags-update-how-to-update)

      (add-hook 'emacs-lisp-mode-hook #'turn-on-ctags-auto-update-mode)
      (add-hook 'verilog-mode-hook #'turn-on-ctags-auto-update-mode))))

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

;;; xref, semantic/symref
;; `xref' using `semantic-symref-detect-symref-tool' and
;; `semantic-symref-calculate-rootdir' to figure out which tool is available
;; for finding definitions and references. It looks for `global', `idutils',
;; and `cscope'. If none of those are found, it defaults to `grep'.
(use-package semantic/symref
  :defer t
  :config
  (progn
    ;; The `semantic-symref-calculate-rootdir' function does not find the
    ;; "right" rootdir by default. So using `projectile-project-root' to do
    ;; that job instead.
    (with-eval-after-load 'projectile
      (defalias 'semantic-symref-calculate-rootdir 'projectile-project-root))))

(bind-keys
 :map modi-mode-map
  ;; Do not set the below binding in `emacs-lisp-mode' buffers because we do
  ;; not want to override the default "M-." binding to `xref-find-definitions'.
 :filter (not (derived-mode-p 'emacs-lisp-mode))
  ("M-." . modi/find-tag))


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
