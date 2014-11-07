;; Time-stamp: <2014-11-07 12:18:01 kmodi>

;; Functions related to editing text in the buffer

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write time stamps when saving files
;; Source: http://www.emacswiki.org/emacs/TimeStamp
;; You can arrange to put a time stamp in a file, so that it is updated
;; automatically each time you edit and save the file. The time stamp must be
;; in the first 8 lines of the file, and you should insert it like this:
;;      Time-stamp: <2013-11-28 02:08:29 KModi>
;; or like this:
;;      Time-stamp: " "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq time-stamp-line-limit 20)
(add-hook 'write-file-hooks 'time-stamp)


;; Now using the drag-stuff package instead
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Moving lines up and down
;; ;; Source: http://www.whattheemacsd.com/
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun move-line-down ()
;;   (interactive)
;;   (let ((col (current-column)))
;;     (save-excursion
;;       (forward-line)
;;       (transpose-lines 1))
;;     (forward-line)
;;     (move-to-column col)))

;; (defun move-line-up ()
;;   (interactive)
;;   (let ((col (current-column)))
;;     (save-excursion
;;       (forward-line)
;;       (transpose-lines -1))
;;     (move-to-column col)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Duplication
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Duplicate current line or region
;; Source: http://tuxicity.se/emacs/elisp/2010/03/11/duplicate-current-line-or-region-in-emacs.html
(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Managing white spaces and empty newlines
;; Delete trailing white space in lines and empty new lines at the end of file
;; at the time of saving
;; This is very useful for macro definitions in Verilog as for multi-line
;; macros, NO space is allowed after line continuation character "\"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'write-file-hooks 'delete-trailing-whitespace)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATE AND TIME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insert date and time
(defvar current-date-time-format "%a %b %d %H:%M:%S %Z %Y"
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")
(defun insert-current-date-time ()
  "insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
  (interactive)
  (insert "==========\n")
  (insert (format-time-string current-date-time-format (current-time)))
  (insert "\n"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Align
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Source: http://stackoverflow.com/questions/6217153/aligning-or-prettifying-code-in-emacs
;; Source: http://stackoverflow.com/questions/3633120/emacs-hotkey-to-align-equal-signs
(defun align-to-equals (begin end)
  "Align region to equal signs"
  (interactive "r")
  ;; align-regexp syntax:  align-regexp (beg end regexp &optional group spacing repeat)
  ;; beg and end are the begin and end of the selected region
  ;; regexp is the default 'group' for deletion/expansion
  ;; group is to specify the parenthesis group that is desired to be deleted/expanded (default=1)
  ;; spacing is the number of spaces that replaces specified parenthesis group (default=0)
  ;; repeat set to t/nil tells whether the alignment needs to be done multiple times per line (default=nil)
  (align-regexp begin end "\\(\\s-*\\)=" 1 1 nil))
;; To do it manually do `M-x align-regexp`, type `=` and hit Enter

;; To perform align-regexp WITHOUT the default values of regexp, group, spacing, repeat
;; do `C-u M-x align-regexp`

(defun align-columns (begin end)
  "Align text columns"
  (interactive "r")
  ;; align-regexp syntax:  align-regexp (beg end regexp &optional group spacing repeat)
  (align-regexp begin end "\\(\\s-+\\)[a-z=(),?':`\.{}]" 1 1 t)
  (indent-region begin end) ;; ident the region correctly after alignment
  )


;; http://stackoverflow.com/questions/3035337/in-emacs-can-you-evaluate-an-emacs-lisp-expression-and-replace-it-with-the-resul
(defun eval-and-replace-last-sexp ()
  "Replace an emacs lisp expression (s-expression aka sexp) with its result"
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%s" value))))
;; How to use: Put the cursor at the end of an expression like (+ 1 2) and
;; `M-x eval-and-replace-last-sexp`


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Toggle comment on current line or selected region
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://stackoverflow.com/questions/9688748/emacs-comment-uncomment-current-line
(defun toggle-comment-on-line-or-region ()
  "comment or uncomment current line or selected region , and go to the next line"
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
  (next-line))

;; Make `kill-whole-line' indentation aware
;; https://github.com/lunaryorn/stante-pede/blob/master/init.el
(defun smart-kill-whole-line (&optional arg)
  "Kill whole line and move back to indentation.
Kill the whole line with function `kill-whole-line' and then move
`back-to-indentation'."
  (interactive "p")
  (kill-whole-line arg)
  (back-to-indentation))

(defun modi/smart-open-line ()
  "Move the current line down if there are no word chars between the start of line
and the cursor. Else, insert empty line after the current line."
  (interactive)
  ;; Get the substring from start of line to current cursor position
  (setq str-before-point (buffer-substring (line-beginning-position) (point)))
  ;; (message "%s" str-before-point)
  (if (not (string-match "\\w" str-before-point))
      (progn (newline-and-indent)
             ;; (open-line 1)
             (previous-line)
             (indent-relative-maybe))
    (progn (move-end-of-line nil)
           (newline-and-indent))))

(defun pull-up-line ()
  "Join the following line onto the current one (analogous to `C-e', `C-d')"
  (interactive)
  (join-line -1))

;; Enable narrowing
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)

;; zap-to-char
;; Source: https://github.com/purcell/emacs.d/blob/master/lisp/init-editing-utils.el
(autoload 'zap-up-to-char "misc" "Kill up to, but not including ARGth occurrence of CHAR.")

;; zop-to-char
;; Source: https://github.com/thierryvolpiatto/zop-to-char
(req-package zop-to-char
  :load-path "from-git/zop-to-char"
  :config
  (progn
    (bind-keys
     :map modi-mode-map
     ("M-z" . zop-up-to-char)
     ("M-Z" . zop-to-char))))

;; indent-guide
(req-package indent-guide
  :config
  (progn
    (setq indent-guide-recursive t)
    (setq indent-guide-char "|")
    (defun turn-on-indent-guide ()
      "Turn on indent-guide-mode only for specific modes."
      (interactive)
      (dolist (hook '(verilog-mode-hook
                      emacs-lisp-mode-hook
                      python-mode-hook
                      sh-mode-hook
                      cperl-mode-hook))
        (add-hook hook 'indent-guide-mode)))
    (defun turn-off-indent-guide ()
      "Turn off indent-guide-mode only for specific modes."
      (interactive)
      (indent-guide-global-mode -1)
      (dolist (hook '(verilog-mode-hook
                      emacs-lisp-mode-hook
                      python-mode-hook
                      sh-mode-hook
                      cperl-mode-hook))
        (remove-hook hook 'indent-guide-mode)))))

;; Aggressive auto indentation
(require 'cl-lib)
(defun endless/indent-defun ()
  "Indent current defun.
Do nothing if mark is active (to avoid deactivaing it), or if
buffer is not modified (to avoid creating accidental
modifications)."
  (interactive)
  (ignore-errors
    (unless (or (region-active-p)
                buffer-read-only
                (null (buffer-modified-p)))
      (let ((l (save-excursion (beginning-of-defun 1) (point)))
            (r (save-excursion (end-of-defun 1) (point))))
        (cl-letf (((symbol-function 'message) #'ignore))
          (indent-region l r))))))

(defun endless/activate-aggressive-indent ()
  "Locally add `endless/indent-defun' to `post-command-hook'."
  (add-hook 'post-command-hook
            #'endless/indent-defun nil 'local))

(add-hook 'emacs-lisp-mode-hook
          #'endless/activate-aggressive-indent)

;; "Insert" copied rectangle instead of overwriting lines as
;; `M-x yank-rectangle` does.

;; Source: http://stackoverflow.com/questions/12165205/how-to-copy-paste-a-region-from-emacs-buffer-with-line-file-reference
(defun copy-with-linenum (beg end use-unicode)
  "Copy the selected region with file name, starting and ending
line numbers, date and user name.

When called with a prefix like `C-u', it will use unicode characters
instead of ASCII characters for adorning the copied snippet."
  (interactive "r\nP")
  (let* ((chars-start-of-code-block ",--------")
         (chars-end-of-code-block   "`--------")
         (chars-n-dash              "--")
         (chars-m-dash              "---")
         (chars-pipe                "|"))
    (save-excursion
      (when use-unicode
        ( setq chars-start-of-code-block "╭────────"
               chars-end-of-code-block   "╰────────"
               chars-n-dash              "─"
               chars-m-dash              "──"
               chars-pipe                "│" ))
      (goto-char end)
      (skip-chars-backward "\n \t")
      (setq end (point))
      (let* ((chunk (buffer-substring beg end)))
        (setq chunk (concat
                     (format "%s #%-d %s %s %s\n%s "
                             chars-start-of-code-block
                             (line-number-at-pos beg)
                             chars-n-dash
                             (or (buffer-file-name) (buffer-name))
                             chars-m-dash
                             chars-pipe)
                     (replace-regexp-in-string "\n"
                                               (format "\n%s "
                                                       chars-pipe)
                                               chunk)
                     (format "\n%s #%-d %s %s %s %s"
                             chars-end-of-code-block
                             (line-number-at-pos end)
                             chars-n-dash
                             (format-time-string "%Y/%m/%d")
                             chars-n-dash
                             (getenv "USER"))))
        (kill-new chunk)))
    (deactivate-mark)))

(with-eval-after-load 'region-bindings-mode
  (bind-keys
   :map region-bindings-mode-map
   ;; When region is selected, pressing `c' will copy the region
   ;; with ASCII character adornment. Pressing `C-u c' will copy
   ;; with Unicode character adornment.
   ("c" . copy-with-linenum)))

;; Key bindings
(bind-keys
 :map modi-mode-map
 ("<f3>"    . toggle-comment-on-line-or-region)
 ("<f4>"    . kmacro-end-or-call-macro) ; end macro recording/call last macro
 ("<S-f4>"  . start-kbd-macro) ; start macro recording
 ("<C-f4>"  . start-kbd-macro) ; start macro recording
 ("<f9>"    . eval-region)
 ("C-x d"   . delete-region)
 ("s-SPC"   . just-one-space) ;; Win-Space
 ("C-S-d"   . duplicate-current-line-or-region)
 ;; override the binding of `C-x =` for `what-cursor-position'
 ("C-x ="   . align-to-equals) ; align all = signs in selected region
 ("C-x \\"  . align-regexp)  ; align selected region to the entered regexp
 ;; align multiple columns in the selected region. Of course all the selected
 ;; lines must have the same number of columns of groups of non-space characters
 ("C-x |"   . align-columns)
 ("C-k"     . kill-line)
 ("C-S-k"   . smart-kill-whole-line)
 ;; override the binding of `C-o` for `open-line'
 ("C-o"     . modi/smart-open-line)
 ("C-j"     . pull-up-line)
 ("M-j"     . comment-indent-new-line)
 ;; Zap!
 ;; ("M-z"     . zap-up-to-char)
 ;; ("M-Z"     . zap-to-char))
 )

(bind-to-modi-map "d" insert-current-date-time)
(bind-to-modi-map "x" eval-and-replace-last-sexp)
;; Bind `what-cursor-position' to `modi-mode-map' as I have overridden its
;; default binding `C-x =' with something else.
(bind-to-modi-map "=" what-cursor-position)

(key-chord-define-global "3e" 'toggle-comment-on-line-or-region) ; alternative to F3
(key-chord-define-global "9o" 'eval-region) ; alternative to F9
(key-chord-define-global "^^" (λ (insert "λ")))


(provide 'setup-editing)


;; TIPS

;; (1) Commented new line
;; `M-j' - `comment-indent-new-line'
;; This creates a commented new line; useful when writing multiline comments
;; like this one without having to manually type in the comment characters.

;; (2) Undo
;; `C-_' or `C-/'
