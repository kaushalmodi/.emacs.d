;; Time-stamp: <2014-03-07 15:10:05 kmodi>

;; ctags, etags

;; Use Exuberant ctags instead of the ctags that comes with emacs.
;; Install Exuberant ctags from http://ctags.sourceforge.net/ and make
;; sure after installing that on doing `ctags --version` in terminal shows that
;; it is the exuberant version and not the emacs version.

;; Don't ask before rereading the TAGS files if they have changed
(setq tags-revert-without-query t)
;; Do case-sensitive tag searches
(setq tags-case-fold-search nil) ;; t=case-insensitive, nil=case-sensitive
;; Don't warn when TAGS files are large
(setq large-file-warning-threshold nil)


(defun build-ctags ()
  (interactive)
  (message "building .emacs.d tags")
  (shell-command (concat "ctags -Re -f " user-emacs-directory "/TAGS " user-emacs-directory "/"))
  (when (boundp 'setup-sos-loaded)
    (message "building project tags")
    (shell-command (concat "ctags -Re -f " project-root "TAGS " project-root)))
  (update-ctags)
  (message "tags built successfully"))

;; Source: https://www.gnu.org/software/emacs/manual/html_node/emacs/Select-Tags-Table.html#Select-Tags-Table
;; You cannot set both tags-file-name (that's what visit-tags-table does) and
;; tags-table-list
(defun update-ctags ()
  (interactive)
  (setq tags-file-name 'nil)
  ;; List all directories from which you want to read the TAGS files
  (if (boundp 'setup-sos-loaded)
      (setq tags-table-list
            `("~/.emacs.d"
              ,project-root))
    (setq tags-table-list
          `("~/.emacs.d")))
  (message "Updated tags-table-list."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; etags-select
;; Source: http://mattbriggs.net/blog/2012/03/18/awesome-emacs-plugins-ctags/
(require 'etags-select)

;; (defun my-find-tag ()
;;   (interactive)
;;   (if (file-exists-p (concat user-emacs-directory "/TAGS"))
;;       (update-ctags)
;;     (build-ctags))
;;   (when (boundp 'setup-sos-loaded)
;;     (if (file-exists-p (concat project-root "TAGS"))
;;         (update-ctags)
;;       (build-ctags)))
;;   (etags-select-find-tag-at-point))

;; ;; Source: http://www.emacswiki.org/EtagsSelect
;; ;; Use ido to list tags, but then select via etags-select (best of both worlds!)
;; (defun my-ido-find-tag ()
;;   "Find a tag using ido"
;;   (interactive)
;;   (tags-completion-table)
;;   (let (tag-names)
;;     (mapatoms (lambda (x)
;;                 (push (prin1-to-string x t) tag-names))
;;               tags-completion-table)
;;     (etags-select-find (ido-completing-read "Tag: " tag-names))))
;; ;; (global-set-key (kbd "M-.") 'my-ido-find-tag)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ac-etags
(require 'ac-etags)

;; Required number of characters of this source completion. You should change
;; this value before calling ac-etags-setup
(setq ac-etags-requires 3) ;; default = 3

(eval-after-load "etags"
  '(progn
      (ac-etags-setup)))

(add-hook 'verilog-mode-hook    'ac-etags-ac-setup)
(add-hook 'matlab-mode-hook     'ac-etags-ac-setup)
(add-hook 'emacs-lisp-mode-hook 'ac-etags-ac-setup)

;; `ac-etags-setup'
;; Setup auto-complete source for etags. This command must be called at the beginning.

;; `ac-etags-ac-setup'
;; Setup etags auto-complete source and enable auto-complete-mode if auto-complete is not enabled.

;; `ac-etags-clear-cache'
;; Clear completions cache. You should clear cache if you switch project and TAGS file.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ctags-update
;; Source: https://github.com/jixiuf/helm-etags-plus

;; Auto update
(setq ctags-update-delay-seconds (* 60 60)) ;; every 1 hour
(autoload 'turn-on-ctags-auto-update-mode "ctags-update" "turn on `ctags-auto-update-mode'." t)
(add-hook 'verilog-mode-hook    'turn-on-ctags-auto-update-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-ctags-auto-update-mode)

;; ;; Manual update
;; (autoload 'ctags-update "ctags-update" "update TAGS using ctags" t)

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


(setq setup-ctags-loaded t)
(provide 'setup-ctags)
