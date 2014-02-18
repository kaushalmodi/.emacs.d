;; Time-stamp: <2014-02-18 11:02:32 kmodi>

;; Miscellaneous config not categorized in other setup-* files

(fset 'yes-or-no-p 'y-or-n-p) ;; Use y or n instead of yes or no

;; Enable conversion of the selected region to upper case using `C-x C-u`
(put 'upcase-region 'disabled nil)

;; Enable conversion of the selected region to lower case using `C-x C-l`
(put 'downcase-region 'disabled nil)

;; Do not make mouse wheel accelerate its action (example: scrolling)
(setq mouse-wheel-progressive-speed nil)

(defun confirm-kill-emacs-save-desktop ()
  "Prevent accidentally killing emacs. Also save current desktop every time when quitting emacs."
  (interactive)
  (if (y-or-n-p-with-timeout "Do you really want to exit Emacs ? " 4 nil)
      (progn
        ;; (when setup-desktop-loaded
        ;;   (session-save t)) ;; save current desktop without asking for confirmation
        (save-buffers-kill-emacs))))

(defun confirm-kill-emacs-dont-save-desktop ()
  "Prevent accidentally killing emacs. Don't save current desktop when quitting emacs."
  (interactive)
  (if (y-or-n-p-with-timeout "Do you really want to exit Emacs without saving desktop? " 4 nil)
      (save-buffers-kill-emacs)))


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
