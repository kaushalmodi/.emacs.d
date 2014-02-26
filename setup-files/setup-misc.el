;; Time-stamp: <2014-02-21 15:46:12 kmodi>

;; Miscellaneous config not categorized in other setup-* files

(fset 'yes-or-no-p 'y-or-n-p) ;; Use y or n instead of yes or no

;; Enable conversion of the selected region to upper case using `C-x C-u`
(put 'upcase-region 'disabled nil)

;; Enable conversion of the selected region to lower case using `C-x C-l`
(put 'downcase-region 'disabled nil)

;; Do not make mouse wheel accelerate its action (example: scrolling)
(setq mouse-wheel-progressive-speed nil)

;; Quitting emacs via `C-x C-c` or the GUI 'X' button
(setq confirm-kill-emacs 'y-or-n-p)

(defun save-desktop-save-buffers-kill-emacs ()
  "Save buffers and current desktop every time when quitting emacs."
  (interactive)
  ;; (when setup-desktop-loaded
  ;;   (session-save t)) ;; save current desktop without asking for confirmation
  (desktop-save-in-desktop-dir)
  (save-buffers-kill-emacs))


(setq setup-misc-loaded t)
(provide 'setup-misc)


;; TIPS

;; (1) Un-define a symbol/variable
;; this will make the symbol my-nasty-variable's value void
;; (makunbound 'my-nasty-variable)

;; (2) Un-define a function
;; this will make the symbol my-nasty-function's
;; function definition void
;; (fmakunbound 'my-nasty-function)

;; (3) See a variable value
;; `C-h v`, enter the variable name, Enter
;; Example: `C-h v`, `tooltip-mode`, Enter

;; (4) How to insert superscript
;; `C-x 8 ^ 2` inserts ²
