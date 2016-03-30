;; Time-stamp: <2016-03-30 11:24:05 kmodi>

;; Emacs Lisp Mode

;; Solution to toggle debug on a function whether it is defined inside or
;; outside a `use-package' wrapper
;; http://emacs.stackexchange.com/q/7643/115

;; Edebug a defun or defmacro
(defvar modi/fns-in-edebug nil
  "List of functions for which `edebug' is instrumented.")

(defconst modi/fns-regexp (concat "(\\s-*"
                                  "\\(cl-\\)*"
                                  "\\(defun\\|defmacro\\|defsubst\\)"
                                  "\\**"
                                  "\\s-+"
                                  "\\(?1:\\(\\w\\|\\s_\\)+\\)\\_>") ; word or symbol char
  "Regexp to find defun or defmacro definition.")

(defun modi/toggle-edebug ()
  (interactive)
  (save-excursion
    (re-search-backward modi/fns-regexp)
    (let ((start (point))
          (fn (match-string 1))
          end
          selection)
      ;; (message "Parsed: %s fns-in-edebug: %s" fn modi/fns-in-edebug)
      (forward-sexp 1)
      (setq end (point))
      (if (member fn modi/fns-in-edebug)
          ;; If the function is already being edebugged, uninstrument it
          (progn
            (setq modi/fns-in-edebug (delete fn modi/fns-in-edebug))
            (eval-buffer)
            (setq-default eval-expression-print-length 12)
            (setq-default eval-expression-print-level  4)
            (message "Edebug disabled: %s" fn))
        ;; If the function is not being edebugged, instrument it
        (save-restriction
          (narrow-to-region start end)
          (add-to-list 'modi/fns-in-edebug fn)
          (setq-default eval-expression-print-length nil)
          (setq-default eval-expression-print-level  nil)
          (edebug-defun)
          (message "Edebug: %s" fn))))))

(defhydra hydra-edebug (:color amaranth
                        :hint  nil)
  "
    EDEBUG MODE
^^_<SPC>_ step             ^^_f_ forward sexp         _b_reakpoint set                previous _r_esult      _w_here                    ^^_d_ebug backtrace
^^_n_ext                   ^^goto _h_ere              _u_nset breakpoint              _e_val expression      bounce _p_oint             _q_ top level (_Q_ nonstop)
_g_o (_G_ nonstop)         ^^_I_nstrument callee      next _B_reakpoint               _E_val list            _v_iew outside             ^^_a_bort recursive edit
_t_race (_T_ fast)         step _i_n/_o_ut            _x_ conditional breakpoint      eval _l_ast sexp       toggle save _W_indows      ^^_S_top
_c_ontinue (_C_ fast)      ^^^^                       _X_ global breakpoint
"
  ("<SPC>" edebug-step-mode)
  ("n"     edebug-next-mode)
  ("g"     edebug-go-mode)
  ("G"     edebug-Go-nonstop-mode)
  ("t"     edebug-trace-mode)
  ("T"     edebug-Trace-fast-mode)
  ("c"     edebug-continue-mode)
  ("C"     edebug-Continue-fast-mode)

  ("f"     edebug-forward-sexp)
  ("h"     edebug-goto-here)
  ("I"     edebug-instrument-callee)
  ("i"     edebug-step-in)
  ("o"     edebug-step-out)

  ;; breakpoints
  ("b"     edebug-set-breakpoint)
  ("u"     edebug-unset-breakpoint)
  ("B"     edebug-next-breakpoint)
  ("x"     edebug-set-conditional-breakpoint)
  ("X"     edebug-set-global-break-condition)

  ;; evaluation
  ("r"     edebug-previous-result)
  ("e"     edebug-eval-expression)
  ("l"     edebug-eval-last-sexp)
  ("E"     edebug-visit-eval-list)

  ;; views
  ("w"     edebug-where)
  ("p"     edebug-bounce-point)
  ("v"     edebug-view-outside) ; maybe obsolete??
  ("P"     edebug-view-outside) ; same as v
  ("W"     edebug-toggle-save-windows)

  ("d"     edebug-backtrace)

  ;; quitting and stopping
  ("q"     top-level :color blue)
  ("Q"     edebug-top-level-nonstop :color blue)
  ("a"     abort-recursive-edit :color blue)
  ("S"     edebug-stop :color blue))
(with-eval-after-load 'edebug
  (bind-key "?" #'hydra-edebug/body edebug-mode-map))

;; Debug on entry
(defvar modi/fns-in-debug nil
  "List of functions for which `debug-on-entry' is instrumented.")

(defun modi/toggle-debug ()
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
;; improve the indentation of blocks like below:
;; (defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
;;                            :color pink
;;                            :post (deactivate-mark))

;; Solution 1
;; (add-hook 'emacs-lisp-mode-hook
;;           (lambda () (setq-local lisp-indent-function #'common-lisp-indent-function)))

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

(defun modi/set-emacs-lisp-indentation ()
  "Customize the indentation for `emacs-lisp-mode'."
  (setq-local lisp-indent-function #'Fuco1/lisp-indent-function))
(add-hook 'emacs-lisp-mode-hook #'modi/set-emacs-lisp-indentation)

;; http://ergoemacs.org/emacs/emacs_byte_compile.html
(defun byte-compile-current-buffer ()
  "`byte-compile' current buffer if in `emacs-lisp-mode' and compiled file exists."
  (interactive)
  (when (and (derived-mode-p 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))
(add-hook 'after-save-hook #'byte-compile-current-buffer)

;; Easy Escape
;; https://github.com/cpitclaudel/easy-escape
(use-package easy-escape
  :config
  (progn
    (setq easy-escape-character ?\\) ; default
    ;; (setq easy-escape-character ?⑊)
    (add-hook 'emacs-lisp-mode-hook #'easy-escape-minor-mode)))

;; Overlay eval results
;; http://endlessparentheses.com/eval-result-overlays-in-emacs-lisp.html
(defun endless/eval-overlay (orig-ret-val)
  "Overlay the eval results"
  (require 'cider-overlays nil :noerror)
  ;; Skip this feature if `cider' is not installed
  (when (fboundp #'cider--make-result-overlay)
    (cider--make-result-overlay (format "%S" orig-ret-val)
      :where (point)
      :duration 'command))
  orig-ret-val)
(advice-add 'eval-last-sexp :filter-return #'endless/eval-overlay)


(provide 'setup-elisp)

;; TIPS

;; You cannot update `defvar' value by `eval-buffer' or `eval-last-sexp'. But
;; you can place the point in each `defvar' form you want to re-evaluate and
;; call `eval-defun' (C-M-x).
