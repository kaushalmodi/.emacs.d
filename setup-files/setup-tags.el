;; -*- lexical-binding: t; -*-
;; Time-stamp: <2019-09-11 12:03:15 kmodi>

;; Setup for different tags

;; Contents:
;;
;;  gtags, GNU global
;;    ggtags
;;  ctags
;;    etags-regen
;;  modi/find-tag
;;  xref, semantic/symref

;;; gtags, GNU global

(when (executable-find "global")
;;;; ggtags
  ;; https://github.com/leoliu/ggtags
  (use-package ggtags
    :config
    (progn
      (setq ggtags-update-on-save nil) ;Don't try to update GTAGS on each save; makes the system sluggish for huge projects.
      (setq ggtags-highlight-tag nil)  ;Don't auto-highlight tag at point.. makes the system really sluggish!
      (setq ggtags-sort-by-nearness nil) ; Enabling nearness requires global 6.5+
      (setq ggtags-navigation-mode-lighter nil)
      (setq ggtags-mode-line-project-name nil)
      (setq ggtags-oversize-limit (* 30 1024 1024)) ; 30 MB

      (dolist (hook '(verilog-mode-hook
                      c-mode-hook
                      c-ts-mode-hook))
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

(defun modi/universal-ctags-p ()
  "Return non-nil if the `ctags' found in PATH is Universal Ctags.
The BSD ctags that ships with macOS cannot write etags-format TAGS files."
  (when (executable-find "ctags")
    (string-match-p "Universal Ctags"
                    (shell-command-to-string "ctags --version 2>/dev/null"))))

(when (modi/universal-ctags-p)
;;;; etags-regen
  ;; Auto-generate the TAGS file for the current project (`project-current')
  ;; and keep it updated as files are saved.
  (use-package etags-regen
    :config
    (progn
      (setq etags-regen-program "ctags")
      ;; File types not covered by the default `etags-regen-file-extensions'.
      (dolist (ext '("sv" "svh" "v" "vh" "tv" "vp" ;Verilog, SystemVerilog
                     "nim" "nims"
                     "tcl"))
        (add-to-list 'etags-regen-file-extensions ext))
      (etags-regen-mode 1))))

;;; modi/find-tag
(defun modi/find-tag (&optional use-xref)
  "Use `ggtags' if available, else use `xref' to find tags.

If USE-XREF is non-nil, use `xref' even when `ggtags' is available."
  (interactive "P")
  (if (or use-xref
          (not (featurep 'ggtags)))
      (call-interactively #'xref-find-definitions)
    (call-interactively #'ggtags-find-tag-dwim)))

;;; xref, semantic/symref
(use-package xref
  :defer t
  :config
  (progn
    ;; Use rg instead of grep for the xref searches that fall back to a
    ;; line-oriented search, like `xref-find-references'.
    (when (executable-find "rg")
      (setq xref-search-program 'ripgrep))))

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
