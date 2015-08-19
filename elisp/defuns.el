;; Time-stamp: <2015-08-18 09:02:17 kmodi>

;; Collection of general purposes defuns and macros

(defmacro >=e (V &rest body)
  "The BODY can contain both
'if'   (emacs version at least version V) and
'else' (emacs version older than V) blocks.

Usage: (>=e \"25.0\"
           (defun-compatible-with-25.0)
         (defun-not-compatible-in-older-version))"
  (declare (indent 2)) ; `if'-style indentation where this macro is used
  `(if (version<= ,V emacs-version)
       ,@body))

;; Alias ^ as a function to calculate exponents
;; (^ 2 15) `C-x C-e' -> 32768
(defalias '^ 'expt)

;; Easier to remember aliases
;; Set symbol's function definition to nil
(defalias 'undefun 'fmakunbound)
;; Set symbol's value to nil
(defalias 'unsetq  'makunbound)
;; Cyclically replace "A" with "X", "Y", "Z", "X", "Y", ..
(defalias 'query-replace-regexp-cyclic 'map-query-replace-regexp)

;; Source https://github.com/Wilfred/ag.el
(defun modi/get-symbol-at-point ()
  "If there's an active selection, return that.
Otherwise, get the symbol at point, as a string."
  (cond ((use-region-p)
         (buffer-substring-no-properties (region-beginning) (region-end)))
        ((symbol-at-point)
         (substring-no-properties
          (symbol-name (symbol-at-point))))))

;; Kill emacs when running in daemon mode or not
;; http://lists.gnu.org/archive/html/emacs-devel/2011-11/msg00348.html
(defun tv-stop-emacs ()
  (interactive)
  (if (daemonp)
      (save-buffers-kill-emacs)
    (save-buffers-kill-terminal)))

;; `with-eval-after-load' macro was introduced in emacs 24.4
;; Below code makes this macro compatible with older versions of emacsen
;; http://www.lunaryorn.com/2013/06/25/introducing-with-eval-after-load.html
(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    `(eval-after-load ,file
       `(funcall (function ,(lambda () ,@body))))))

;; http://emacs.stackexchange.com/a/5343/115
(defun modi/blend-fringe ()
  (interactive)
  "Set the fringe foreground and background color to that of the theme."
  (set-face-attribute 'fringe nil
                      :foreground (if (string= (face-foreground 'default) "unspecified-fg")
                                      "#f7f7f7" (face-foreground 'default))
                      :background (if (string= (face-background 'default) "unspecified-bg")
                                      "#282828" (face-background 'default))))

;; Re-evaluatable `defvar's
;; Usage: When debugging/developing something in elisp, it is useful to have
;; the `defvar's re-evaluatable. To do so, temporarily set the
;; `defvar-always-reeval-values' to `t' and use `defvar-re's instead of
;; `defvar's.
;; It is recommended to keep the default value of the above var as nil.
;; http://emacs.stackexchange.com/a/2301/115

;; When non-nil, defvar will reevaluate the init-val arg even if the symbol
;; is defined.
(setq-default defvar-always-reeval-values nil)

(defmacro defvar-re (name &optional init-value docstring)
  "Like defvar, but when `defvar-always-reeval-values' is non-nil, it will
set the symbol's value to INIT-VALUE even if the symbol is defined."
  `(progn
     (when defvar-always-reeval-values
       (makunbound ',name))
     (defvar ,name ,init-value ,docstring)))

;; Fontify `defvar-re' as emacs-lisp keyword
(font-lock-add-keywords
 'emacs-lisp-mode
 '(("(\\(defvar\\-re\\)\\_> +\\(.*?\\)\\_>"
    (1 font-lock-keyword-face)
    (2 font-lock-type-face))))

(defun modi/toggle-defvar-re-eval ()
  "Toggle the “re-evaluatable” state of `defvar's."
  (interactive)
  (if defvar-always-reeval-values
      (progn
        (setq-default defvar-always-reeval-values nil)
        (message "'defvar-re' will now work as 'defvar'."))
    (progn
      (setq-default defvar-always-reeval-values t)
      (message "'defvar-re' will now force re-evaluate."))))
;;


(provide 'defuns)
