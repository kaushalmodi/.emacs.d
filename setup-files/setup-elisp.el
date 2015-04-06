;; Time-stamp: <2015-04-06 12:12:20 kmodi>

;; Emacs Lisp Mode

;; Solution to toggle debug on a function whether it is defined inside or
;; outside a `use-package' wrapper
;; http://emacs.stackexchange.com/q/7643/115

;; Edebug a defun or defmacro
(defvar modi/fns-in-edebug nil
  "List of functions for which `edebug' is instrumented.")

(defconst modi/fns-regexp (concat "(\\s-*"
                                  "\\(defun\\|defmacro\\)\\s-+"
                                  "\\(?1:\\(\\w\\|\\s_\\)+\\)\\_>") ; word or symbol char
  "Regexp to find defun or defmacro definition.")

(defun modi/toggle-edebug-defun ()
  (interactive)
  (let (fn)
    (save-excursion
      (re-search-backward modi/fns-regexp)
      (setq fn (match-string 1))
      ;; (message "Parsed: %s fns-in-edebug: %s" fn modi/fns-in-edebug)
      (mark-sexp)
      (narrow-to-region (point) (mark))
      (if (member fn modi/fns-in-edebug)
          ;; If the function is already being edebugged, uninstrument it
          (progn
            (setq modi/fns-in-edebug (delete fn modi/fns-in-edebug))
            (eval-region (point) (mark))
            (setq-default eval-expression-print-length 12)
            (setq-default eval-expression-print-level  4)
            (message "Edebug disabled: %s" fn))
        ;; If the function is not being edebugged, instrument it
        (progn
          (add-to-list 'modi/fns-in-edebug fn)
          (setq-default eval-expression-print-length nil)
          (setq-default eval-expression-print-level  nil)
          (edebug-defun)
          (message "Edebug: %s" fn)))
      (widen))))

;; Debug on entry
(defvar modi/fns-in-debug nil
  "List of functions for which `debug-on-entry' is instrumented.")

(defun modi/toggle-debug-defun ()
  (interactive)
  (let (fn)
    (save-excursion
      (re-search-backward modi/fns-regexp)
      (setq fn (match-string 1)))
    (if (member fn modi/fns-in-debug)
        ;; If the function is already being debugged, cancel its debug on entry
        (progn
          (setq modi/fns-in-debug (delete fn modi/fns-in-debug))
          (cancel-debug-on-entry (intern fn))
          (message "Debug-on-entry disabled: %s" fn))
      ;; If the function is not being debugged, debug it on entry
      (progn
        (add-to-list 'modi/fns-in-debug fn)
        (debug-on-entry (intern fn))
        (message "Debug-on-entry: %s" fn)))))

;; Turn on ElDoc mode
(dolist ( hook '(emacs-lisp-mode-hook
                 lisp-interaction-mode-hook
                 ielm-mode-hook
                 eval-expression-minibuffer-setup-hook))
  (add-hook hook #'eldoc-mode))

;; Change the default indentation function for `emacs-lisp-mode' to
;; improve the indentation of blocks like:
;; (defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
;;                            :color pink
;;                            :post (deactivate-mark))

;; Solution 1
;; (add-hook 'emacs-lisp-mode-hook
;;           (λ (setq-local lisp-indent-function #'common-lisp-indent-function)))

;; Solution 2
;; http://emacs.stackexchange.com/q/10230/115
;; https://github.com/Fuco1/.emacs.d/blob/af82072196564fa57726bdbabf97f1d35c43b7f7/site-lisp/redef.el#L12-L94
(defun Fuco1/lisp-indent-function (indent-point state)
  "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.

INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:

* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);

* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;

* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.

This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
  (let ((normal-indent (current-column))
        (orig-point (point)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (cond
     ;; car of form doesn't seem to be a symbol, or is a keyword
     ((and (elt state 2)
           (or (not (looking-at "\\sw\\|\\s_"))
               (looking-at ":")))
      (if (not (> (save-excursion (forward-line 1) (point))
                  calculate-lisp-indent-last-sexp))
          (progn (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point)
                                     calculate-lisp-indent-last-sexp 0 t)))
      ;; Indent under the list or under the first sexp on the same
      ;; line as calculate-lisp-indent-last-sexp.  Note that first
      ;; thing on that line has to be complete sexp since we are
      ;; inside the innermost containing sexp.
      (backward-prefix-chars)
      (current-column))
     ((and (save-excursion
             (goto-char indent-point)
             (skip-syntax-forward " ")
             (not (looking-at ":")))
           (save-excursion
             (goto-char orig-point)
             (looking-at ":")))
      (save-excursion
        (goto-char (+ 2 (elt state 1)))
        (current-column)))
     (t
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            method)
        (setq method (or (function-get (intern-soft function)
                                       'lisp-indent-function)
                         (get (intern-soft function) 'lisp-indent-hook)))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method indent-point state))))))))
(add-hook 'emacs-lisp-mode-hook
          (λ (setq-local lisp-indent-function #'Fuco1/lisp-indent-function)))

;; http://ergoemacs.org/emacs/emacs_byte_compile.html
(defun byte-compile-current-buffer ()
  "`byte-compile' current buffer if in `emacs-lisp-mode' and compiled file exists."
  (interactive)
  (when (and (derived-mode-p 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))
(add-hook 'after-save-hook #'byte-compile-current-buffer)


(provide 'setup-elisp)
