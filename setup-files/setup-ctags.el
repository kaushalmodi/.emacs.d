;; Time-stamp: <2014-08-13 01:13:50 kmodi>

;; ctags, etags

;; Use Exuberant ctags instead of the ctags that comes with emacs.
;; Install Exuberant ctags from http://ctags.sourceforge.net/ and make
;; sure after installing that on doing `ctags --version` in terminal shows that
;; it is the exuberant version and not the emacs version.

;; Don't ask before rereading the TAGS files if they have changed
(setq tags-revert-without-query t)
;; Do case-sensitive tag searches
(setq tags-case-fold-search nil) ;; t=case-insensitive, nil=case-sensitive
;; Increase the warning threshold to be more than normal TAGS file sizes
(setq large-file-warning-threshold 30000000) ;; 30MB

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; etags-table
;; Depending on the location of the file in buffer, the respective TAGS file is
;; opened on doing a tag find.
(req-package etags-table
  :config
  (progn
    (setq etags-table-alist
          (list
           `(,(concat user-emacs-directory "/.*") ,(concat user-emacs-directory "/TAGS")))) ;; emacs config

    (when (boundp 'uvm-source-code-dir) ;; add-to-list if uvm-source-code-dir symbol is defined
      (add-to-list 'etags-table-alist
                   `(,(concat uvm-source-code-dir "/.*") ,(concat uvm-source-code-dir "/TAGS")) t))

    (setq etags-table-search-up-depth 15) ;; Max depth to search up for a tags file.  nil means don't search.

    ;; Below function comes useful when you change the project-root symbol to a
    ;; different value (when switching projects)
    (defun update-etags-table-then-find-tag ()
      "Update etags-table based on the current value of project-root and then do
tag find"
      (interactive)
      (when (boundp 'project-root) ;; add-to-list if project-root symbol is defined
        (add-to-list 'etags-table-alist
                     `(,(concat project-root "/.*") ,(concat project-root "/TAGS")) t)
        (when (boundp 'uvm-source-code-dir)
          (add-to-list 'etags-table-alist
                       `("/pr.*j.*/.*" ,(concat uvm-source-code-dir "/TAGS")) t)))
      (etags-select-find-tag-at-point))
    (bind-keys
     :map modi-mode-map
     ("M-.") . update-etags-table-then-find-tag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; etags-select
;; Source: http://mattbriggs.net/blog/2012/03/18/awesome-emacs-plugins-ctags/
(req-package etags-select
  :config
  (progn
    (bin-keys
     :map etags-select-mode-map
     ("C-g"   . etags-select-quit)
     ;; Also quit etags-select when cursor moves to another window
     ("C-x o" . etags-select-quit)
     ("C-x O" . etags-select-quit)
     ("C-p"   . etags-select-previous-tag)
     ("C-n"   . etags-select-next-tag))
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
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ctags-update
;; Source: https://github.com/jixiuf/helm-etags-plus
(req-package ctags-update
  :commands (turn-on-ctags-auto-update-mode)
  :config
  (progn
    ;; Auto update
    (setq ctags-update-delay-seconds (* 30 60)) ;; every 1/2 hour
    (add-hook 'verilog-mode-hook    'turn-on-ctags-auto-update-mode)
    (add-hook 'emacs-lisp-mode-hook 'turn-on-ctags-auto-update-mode)
    ;; ;; Manual update
    ;; (autoload 'ctags-update "ctags-update" "update TAGS using ctags" t)
    ))

;;  `ctags-update'
;;    update TAGS in parent directory using `exuberant-ctags'.
;;  `ctags-auto-update-mode'
;;    auto update TAGS using `exuberant-ctags' in parent directory.
;;  `turn-on-ctags-auto-update-mode'
;;    turn on `ctags-auto-update-mode'.


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; helm-etags+
;; ;; Source: https://github.com/jixiuf/helm-etags-plus

;; (require 'helm-etags+)

;; ;; dev suggested setting below variables to avoid the issue I posted on his
;; ;; github: https://github.com/jixiuf/helm-etags-plus/issues/9
;; (setq find-file-visit-truename nil)
;; (setq helm-etags+-follow-symlink-p nil)


(provide 'setup-ctags)


;; NOTES

;; (1) Emacs rereads the TAGS file during every tag find operation.
