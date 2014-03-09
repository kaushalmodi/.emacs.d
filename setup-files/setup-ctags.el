;; Time-stamp: <2014-03-09 04:57:01 kmodi>

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

;; List all directories from which you want to read the TAGS files
(setq tags-table-list '("~/.emacs.d"))

(if (boundp 'setup-sos-loaded)
    (add-to-list 'tags-table-list project-root t))

(defun build-ctags ()
  (interactive)
  (loop for dir in tags-table-list do
        (message (concat "building tags in " dir))
        (shell-command (concat "ctags -Re -f " dir "/TAGS " dir)))
  (message "tags built successfully"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; etags-select
;; Source: http://mattbriggs.net/blog/2012/03/18/awesome-emacs-plugins-ctags/
(require 'etags-select)

(define-key etags-select-mode-map (kbd "C-g")   'etags-select-quit)
;; Also quit etags-select when cursor moves to another window
(define-key etags-select-mode-map (kbd "C-x o") 'etags-select-quit)
(define-key etags-select-mode-map (kbd "C-p")   'etags-select-previous-tag)
(define-key etags-select-mode-map (kbd "C-n")   'etags-select-next-tag)
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
(require 'ctags-update)

;; Auto update
(setq ctags-update-delay-seconds (* 30 60)) ;; every 1/2 hour
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm-etags+
;; Source: https://github.com/jixiuf/helm-etags-plus

(require 'helm-etags+)

;; dev suggested setting below variables to avoid the issue I posted on his
;; github: https://github.com/jixiuf/helm-etags-plus/issues/9
(setq find-file-visit-truename nil)
(setq helm-etags+-follow-symlink-p nil)


(setq setup-ctags-loaded t)
(provide 'setup-ctags)


;; NOTES

;; (1) Emacs reread the TAGS file during every tag find operation.
