;; Time-stamp: <2020-09-01 13:44:20 kmodi>

;; Functions related to editing text in the buffer
;; Contents:
;;
;;  Coding System
;;  Time stamps
;;    Insert time-stamp + user name
;;  Clipboard
;;  Delete Selection
;;  Show Paren
;;  Duplicate current line or region
;;  Managing white spaces and empty newlines
;;  Tabs and Untabify
;;  Align
;;  Eval and replace last sexp
;;  My modified basic functions
;;    Kill Line
;;    Open Line
;;    Pull Up Line
;;  Enable the disabled functions
;;    Narrowing
;;    Setting Goal Column
;;    Upper/lower case conversion
;;  zop-to-char
;;  Kill with line numbers
;;  Rectangle
;;  Cycle Letter Case
;;  Sort Words
;;  Fill/unfill
;;  Replace identical strings with incremental number suffixes
;;  Delete Blank Lines
;;  Space Adjustment After Word Kills
;;  Operate on Region or Whole Buffer
;;  Mark Management
;;    Popping marks
;;    Smart Mark
;;  Tweaking `region-extract-function'
;;  Commenting
;;  Anonymize
;;  Keep Lines - Because I Said So
;;  Bindings

;;; Coding System
(setq keyboard-coding-system 'utf-8-unix)

;;; Time stamps
;; Write time stamps when saving files
;; http://www.emacswiki.org/emacs/TimeStamp
;; You can arrange to put a time stamp in a file, so that it is updated
;; automatically each time you edit and save the file. The time stamp must be
;; in the first few N lines of the file (where N is set by the variable
;; `time-stamp-line-limit', and you should insert it like this:
;;      Time-stamp: <2013-11-28 02:08:29 KModi>
;; or like this:
;;      Time-stamp: " "
(setq time-stamp-line-limit 20)
(add-hook 'before-save-hook #'time-stamp)

;;;; Insert time-stamp + user name
;; http://emacs.stackexchange.com/a/17358/115
(defun modi/insert-time-stamp (option)
  "Insert time-stamp with user name - DWIM.

If the point is not in a comment or string,
  time-stamp + user name is inserted prefixed with `comment-start' characters.

If the point is in a comment or string immediately after `comment-start' chars,
or if the buffer's major-mode has `comment-start' set to nil,
  time-stamp + user name is inserted without any prefix.

If the point is in a comment or string, but *not* immediately after
`comment-start' chars,
  time-stamp + user name is inserted with ‘--’ prefix.

Additional control:

        C-u -> Prefix (`comment-start' or ‘--’) is not auto-inserted.
    C-u C-u -> User name is not inserted.
          0 -> Neither prefix nor user name is inserted."
  (interactive "p")
  (cl-flet ((in-string-or-comment-p () (or (nth 3 (syntax-ppss))
                                           (nth 4 (syntax-ppss)))) ; (elisp) Parser State
            (bol-after-space-maybe () (looking-back "^ *"))
            (eol-before-space-maybe () (looking-at " *$"))
            (after-space-p () (looking-back " ")))
    (let ((current-date-time-format "%a %b %d %H:%M:%S %Z %Y")
          (no-prefix (or (= option 4) ; C-u or C-u C-u C-u
                         (= option 0)))
          (no-user-name (or (= option 16) ; C-u C-u or C-u C-u C-u
                            (= option 0))))
      ;; Do not insert a space if point is at beginning of line (followed by
      ;; optional space) or if there is already space before the current point.
      (unless (or (bol-after-space-maybe)
                  (after-space-p))
        (insert " "))
      ;; Do not insert prefix if `comment-start' is undefined for the major mode
      ;; or if `no-prefix' is non-nil.
      (unless (or no-prefix
                  (not (stringp comment-start)))
        (if (in-string-or-comment-p)
            ;; Do not insert "--" if the point is immediately after
            ;; `comment-start' chars (followed by optional space) or at
            ;; beginning of line (followed by optional space).
            (unless (or (looking-back (concat comment-start " *"))
                        (bol-after-space-maybe))
              (insert "--"))
          ;; If the point is NOT in a comment or string, insert `comment-start'.
          ;; Only for `emacs-lisp-mode', insert 2 semi-colons when point is at
          ;; the beginning of the line.
          (when (and (derived-mode-p 'emacs-lisp-mode)
                     (bol-after-space-maybe))
            (insert comment-start)) ; insert first of the 2 semi-colons
          (insert comment-start)))
      ;; Do not insert a space if the point is at the beginning of line
      ;; (followed by optional space) or if the point is after a space.
      (unless (or (bol-after-space-maybe)
                  (after-space-p))
        (insert " "))
      (insert (format-time-string current-date-time-format (current-time)))
      (unless no-user-name
        (insert " - " user-login-name))
      ;; Do not insert a space after the time stamp if at the end of the line
      ;; (preceding optional space).
      (unless (eol-before-space-maybe)
        (insert " ")))))
(bind-key "C-c D" #'modi/insert-time-stamp modi-mode-map)

;;; Clipboard
;; Non-nil means cutting and pasting uses the clipboard.  This can be in
;; addition to, but in preference to, the primary selection, if applicable (i.e.
;; under X11).
(>=e "25.0"
    (setq select-enable-clipboard t)    ; default = t
  (setq x-select-enable-clipboard t))
;; Non-nil means cutting and pasting uses the primary selection
;; The existence of a primary selection depends on the underlying GUI you use.
;; E.g. it doesn't exist under MS-Windows.
;; Wed Sep 21 15:04:49 EDT 2016 - kmodi
;; Set `select-enable-primary' to non-nil as well so that any text copied/cut
;; within emacs gets copied to the primary too. For example, any text that
;; gets saved to the kill-ring using the `kill-new' function.
(>=e "25.0"
    (setq select-enable-primary t)    ; default = nil
  (setq x-select-enable-primary t))

;; Save text copied from an external program to the kill ring before killing
;; new text from within emacs.
(setq save-interprogram-paste-before-kill t)

;;; Delete Selection
;; Typing anything after highlighting text overwrites that text
;; http://emacsredux.com/blog/2013/04/12/delete-selection-on-insert
(delete-selection-mode 1)

;;; Show Paren
;; Allow one to see matching pairs of parentheses
;; When point is on one of the paired characters, highlight the other
(show-paren-mode 1)

;;; Duplicate current line or region
;; http://tuxicity.se/emacs/elisp/2010/03/11/duplicate-current-line-or-region-in-emacs.html
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

;;; Managing white spaces and empty newlines
(setq require-final-newline t)

;; Fri May 06 17:55:12 EDT 2016 - kmodi
;; Below workaround is needed to make `modi/delete-trailing-whitespace-buffer'
;; to work correctly in org capture buffers, which are Indirect Buffers.
;;   When saving the org capture (indirect) buffers in-between edits, the
;; `modi/delete-trailing-whitespace-buffer' function that runs in the
;; `before-save-hook' would apply around the point in the BASE buffer, NOT
;; around where the point currently is in the indirect org capture buffer.
;; This happens where the current buffer is set to the (buffer-base-buffer) in
;; `basic-save-buffer' (which is called in `save-buffer'):
;;     (if (buffer-base-buffer)
;;         (set-buffer (buffer-base-buffer))) ; <-- The point moves around here!
;; The workaround is to save the mark as soon as `basic-save-buffer' is called
;; (only when the current buffer is an org capture buffer), and then pop to that
;; saved mark at the very beginning of `modi/delete-trailing-whitespace-buffer'.
(defvar modi/org-capture-buffer-file-name nil
  "Variable to save the org capture buffer file name.")
(defun modi/advice-basic-save-buffer-save-mark (&rest args)
  "Save the mark just before calling ORIG-FUN.

Do this only if the current buffer is an org capture buffer."
  (let ((buf (buffer-name)))
    ;; Push mark to the mark ring ONLY in org capture buffers
    (when (string-match "\\`CAPTURE-\\(.*\\.org\\)\\'" buf)
      (setq modi/org-capture-buffer-file-name (match-string-no-properties 1 buf))
      (push-mark (point)))))
(advice-add 'basic-save-buffer :before #'modi/advice-basic-save-buffer-save-mark)
;; (advice-remove 'basic-save-buffer #'modi/advice-basic-save-buffer-save-mark)

;; Delete trailing white space in lines and empty new lines at the end of file
;; when saving files. This is very useful for macro definitions in Verilog as for
;; multi-line macros, NO space is allowed after line continuation character "\".
(defvar-local do-not-delete-trailing-whitespace nil
  "If non-nil, `modi/delete-trailing-whitespace-buffer' will do nothing.

This may also be a function to test if `modi/delete-trailing-whitespace-buffer'
should run.

If you do not want to automatically delete the trailing whitespace in
any of the files in a given sub-directory, create a `.dir-locals.el' file in
that sub-directory will the below contents:

    ((\"sub-dir-name\"
      . ((nil . ((do-not-delete-trailing-whitespace . t))))))

Another way is to set the default value of this variable to a test function.
This is useful where a public project might already have a .dir-locals.el and
you do not want to modify that.

Example:

    (defun modi/prevent-trailing-whitespace-deletion-p ()
      \"Return non-nil for projects where trailing whitespace should not be deleted.\"
      (if-let* ((proj-root-dir (cdr (project-current)))) ;Requires emacs 25.1
          (string-match-p (concat \"\\\\(\"
                                  \"git/emacs/\"
                                  \"\\\\|/org-mode/\"
                                  \"\\\\)$\")
                          proj-root-dir)
        nil))
    (setq-default do-not-delete-trailing-whitespace
                  #'modi/prevent-trailing-whitespace-deletion-p)
")

;; http://stackoverflow.com/a/35781486/1219634
(defun modi/delete-trailing-whitespace-buffer ()
  "Delete trailing whitespace in the whole buffer.

If the major-mode is org-mode, and if the point is after a heading or list
marker, do that in the whole buffer *except* on the current line. The reason is
that if I am about to type a heading (\"* |\"), and if I save the file, I do not
want my cursor to move back (\"*|\").

Do not do anything if `do-not-delete-trailing-whitespace' is non-nil."
  (interactive)
  (unless (if (functionp do-not-delete-trailing-whitespace)
              (funcall do-not-delete-trailing-whitespace)
            do-not-delete-trailing-whitespace)
    (let ((pre-marker-restore-point (point)))
      ;; Only in org capture buffers, first restore the point to the marker
      ;; saved by the `modi/advice-basic-save-buffer-save-mark' function.
      (when (string= modi/org-capture-buffer-file-name (buffer-name))
        (pop-to-mark-command)
        (message "delete-trailing-whitespace: Restored point to %S from %S."
                 pre-marker-restore-point (point))
        ;; Also set the value of modi/org-capture-buffer-file-name back to nil
        ;; to prevent false executions of this `when' form.
        (setq modi/org-capture-buffer-file-name nil)))
    (if (and (derived-mode-p 'org-mode)
             (looking-back (concat
                            "^\\(?:"
                            ;; "* ", "** "
                            "\\*+"
                            ;; "** TODO "
                            ;; If `org-todo-keywords' is '((sequence "TODO" "DONE")),
                            ;; the regexp will be ""\\<\\(DONE\\|TODO\\)\\>""
                            "\\(?:\\s-" (regexp-opt (cdr (car org-todo-keywords)) 'words)
                            "\\)*"
                            ;; "- ", " - ", "+ ", " + "
                            "\\|" "[[:blank:]]*[-+]"
                            "\\)"
                            "\\s-")
                           (save-excursion ;Get the BOL point
                             (beginning-of-line)
                             (point))))
        (progn
          (delete-trailing-whitespace (point-min) (line-beginning-position))
          ;; Below, the END argument is nil so that trailing empty lines are
          ;; also deleted if `delete-trailing-lines' is non-nil.
          (delete-trailing-whitespace (line-end-position) nil))
      (delete-trailing-whitespace (point-min) nil))))
(add-hook 'before-save-hook #'modi/delete-trailing-whitespace-buffer)
;; (remove-hook 'before-save-hook #'modi/delete-trailing-whitespace-buffer)

;;; Tabs and Untabify
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)  ;Use spaces instead of tabs for indentation
(defun modi/untabify-buffer ()
  "Untabify the current buffer.

http://www.veripool.org/issues/345-Verilog-mode-can-t-get-untabify-on-save-to-work
Note that the function's return value is set to nil because if this function is
added to `write-file-functions' hook, emacs will stay stuck at at the
\"Saving file ..\" message and the file won't be saved if any function added to
`write-file-functions' returned a non-nil value.

As per the suggestion in https://debbugs.gnu.org/cgi/bugreport.cgi?bug=21492,
for this purpose, it makes a better sense to use `before-save-hook' (a normal
hook) instead of `write-file-functions' (an abnormal hook that relies on stuff
like the function return values).

So below would be a recommended way of using this function:

    (defun modi/verilog-mode-customization ()
      (add-hook 'before-save-hook #'modi/untabify-buffer nil :local))
    (add-hook 'verilog-mode-hook #'modi/verilog-mode-customization)

Note that it is suggested to add this function to the `before-save-hook'
*locally* within a hook for a major mode which does not require the use of
tabs instead of spaces. Do NOT add this function to the hook globally,
because it can cause issues with files like Makefiles that rely on the use of
tabs explicitly."
  (interactive)
  (untabify (point-min) (point-max))
  ;; Return nil for the benefit of `write-file-functions'.
  nil)

;;; Align
;; http://stackoverflow.com/questions/6217153/aligning-or-prettifying-code-in-emacs
;; http://stackoverflow.com/questions/3633120/emacs-hotkey-to-align-equal-signs
(defun modi/align-to-equals (begin end)
  "Align region to equal signs"
  (interactive "r")
  ;; align-regexp syntax:  align-regexp (beg end regexp &optional group spacing repeat)
  ;; beg and end are the begin and end of the selected region
  ;; regexp is the default 'group' for deletion/expansion
  ;; group is to specify the parenthesis group that is desired to be deleted/expanded (default=1)
  ;; spacing is the number of spaces that replaces specified parenthesis group (default=0)
  ;; repeat set to t/nil tells whether the alignment needs to be done multiple times per line (default=nil)
  (align-regexp begin end "\\([[:blank:]]*\\)=" 1 1 nil))
;; To do it manually do `M-x align-regexp`, type `=` and hit Enter

;; To perform align-regexp WITHOUT the default values of regexp, group, spacing, repeat
;; do `C-u M-x align-regexp`

(defun modi/align-columns (begin end)
  "Align text columns"
  (interactive "r")
  ;; align-regexp syntax:  align-regexp (beg end regexp &optional group spacing repeat)
  (align-regexp begin end "\\([[:blank:]]+\\)[[:alnum:]=(),?':`\.{}]" 1 1 t)
  (indent-region begin end)) ; indent the region correctly after alignment

;;; Eval and replace last sexp
;; http://stackoverflow.com/a/3035574/1219634
(defun eval-and-replace-last-sexp ()
  "Replace an emacs lisp expression (s-expression aka sexp) with its result.

How to use: Put the cursor at the end of an expression like (+ 1 2) and call
this command."
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%s" value))))
(bind-to-modi-map "x" #'eval-and-replace-last-sexp)

;;; My modified basic functions

;;;; Kill Line
(defun modi/kill-line (&optional arg)
  "Kill line.

If ARG is nil, kill line from the current point to the end of the line, not
including the trailing newline. But if the point is at the beginning of the
line or at indentation, kill the whole line including the trailing newline
and move back to the indentation.

If ARG <  0, kill lines backward.

If ARG == 0, kill text before point on the current line.

If ARG >  0, kill that many lines from point."
  (interactive "P")
  (when (and arg (listp arg)) ; to handle universal prefix cases
    (setq arg (car arg)))
  (let* ((kill-whole-line t)
         (bol-point (save-excursion
                      (beginning-of-line)
                      (point)))
         (bol-or-at-indentation-p (looking-back "^ *" bol-point)))
    (when bol-or-at-indentation-p
      (beginning-of-line))
    (kill-line arg)
    (when bol-or-at-indentation-p
      (back-to-indentation))))

;;;; Open Line
(defun modi/smart-open-line (&optional n)
  "Move the current line down if there are no word chars between the start of
line and the cursor. Else, insert empty line after the current line."
  (interactive "p")
  (if (derived-mode-p 'org-mode)
      (dotimes (cnt n)
        (org-open-line 1))
    ;; Get the substring from start of line to current cursor position
    (let ((str-before-point (buffer-substring (line-beginning-position) (point))))
      ;; (message "%s" str-before-point)
      (if (not (string-match "\\w" str-before-point))
          (progn
            (dotimes (cnt n)
              (newline-and-indent))
            ;; (open-line 1)
            (previous-line n)
            (indent-relative-maybe))
        (progn
          (move-end-of-line nil)
          (dotimes (cnt n)
            (newline-and-indent))
          (previous-line (- n 1)))))))

;;;; Pull Up Line
;; http://emacs.stackexchange.com/q/7519/115
(defun modi/pull-up-line ()
  "Join the following line onto the current one.

This is analogous to \\[move-end-of-line] followed by
\\[delete-foward], or \\[universal-argument] \\[delete-indentation],
or \\[universal-argument] \\[join-line].

If the current line is a comment and the pulled-up line is also a
comment, remove the leading comment characters from that line."
  (interactive)
  (join-line -1)
  (when (nth 4 (syntax-ppss))           ;If the current line is a comment
    ;; Remove comment prefix chars from the pulled-up line if present.
    (save-excursion
      ;; Delete all comment-start and space characters, one at a time.
      (while (looking-at (concat "\\s<"  ;Comment-start char as per syntax table
                                 "\\|" (substring comment-start 0 1) ;First char of `comment-start'
                                 "\\|" "[[:blank:]]"))               ;Extra spaces
        (delete-forward-char 1))
      (insert-char ? ))))               ;Insert space

;;; Enable the disabled functions

;;;; Narrowing
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)

;;;; Setting Goal Column
;; http://emacsblog.org/2007/03/17/quick-tip-set-goal-column/
(put 'set-goal-column  'disabled nil)
;;     C-x C-n <- Set goal column
;; C-u C-x C-n <- Unset goal column

;;;; Upper/lower case conversion
(put 'upcase-region   'disabled nil)
(put 'downcase-region 'disabled nil)

;;; zop-to-char
;; https://github.com/thierryvolpiatto/zop-to-char
(use-package zop-to-char
  :config
  (progn
    (setq zop-to-char-kill-keys          '(?\r ?\C-k))
    (setq zop-to-char-copy-keys          '(?\M-w ?\C-c))
    (setq zop-to-char-next-keys          '(?\C-n ?\C-f))
    (setq zop-to-char-prec-keys          '(?\C-p ?\C-b))
    (setq zop-to-char-quit-at-pos-keys   '(?\C-g ?\e)) ; quit to original pos
    (setq zop-to-char-quit-at-point-keys '(?\C-q nil)) ; quit to current pos
    (setq zop-to-char-erase-keys         '(?\d ?\C-d))
    (bind-keys
     :map modi-mode-map
     ("M-z" . zop-up-to-char)
     ("M-Z" . zop-to-char))))

;;; Kill with line numbers
;; http://stackoverflow.com/q/12165205/1219634
(defun modi/kill-with-linenum (beg end unicode)
  "Copy the selected region with file name, starting and ending
line numbers, date and user name.

When called with a prefix like \\[universal-argument], it will use
unicode characters instead of ASCII characters for adorning the
copied snippet."
  (interactive "r\nP")
  (save-excursion
    (goto-char end)
    (skip-chars-backward "\n \t")
    (setq end (point))
    (let ((chars-start-of-code-block (if unicode "╭────────" ",--------"))
          (chars-end-of-code-block   (if unicode "╰────────" "`--------"))
          (chars-n-dash              (if unicode "─"         "--"       ))
          (chars-m-dash              (if unicode "──"        "---"      ))
          (chars-pipe                (if unicode "│"         "|"        ))
          (chunk                     (buffer-substring beg end))
          (buffer-or-file-name (or (buffer-file-name) (buffer-name))))
      (setq chunk (concat
                   (format "%s #%-d %s %s %s\n%s "
                           chars-start-of-code-block
                           (line-number-at-pos beg)
                           chars-n-dash
                           (replace-regexp-in-string ;foo_<USER> -> foo_$USER
                            (concat "_" user-login-name) "_$USER"
                            (replace-regexp-in-string ;/proj/foo/<USER>/ -> /proj/foo/$USER/
                             (concat "\\(/proj/[^/]+/\\)" user-login-name "/") "\\1$USER/"
                             buffer-or-file-name))
                           chars-m-dash
                           chars-pipe)
                   (replace-regexp-in-string
                    "\n" (format "\n%s " chars-pipe) chunk)
                   (format "\n%s #%-d %s %s %s %s"
                           chars-end-of-code-block
                           (line-number-at-pos end)
                           chars-n-dash
                           (format-time-string "%Y/%m/%d")
                           chars-n-dash
                           user-login-name)))
      (kill-new chunk)))
  (deactivate-mark))

(defun modi/kill-with-linenum-unicode (beg end)
  (interactive "r")
  (modi/kill-with-linenum beg end :unicode))

(bind-keys
 :map region-bindings-mode-map
 ;; When region is selected, pressing `c' will copy the region
 ;; with ASCII character adornment.
 ;; Pressing `C-u c' or `C' will copy with Unicode character adornment.
 ("c" . modi/kill-with-linenum)
 ("C" . modi/kill-with-linenum-unicode))

;;; Rectangle

;; How to position the cursor after the end of line; useful for copying/killing
;; rectangles have lines of varying lengths.
;; http://emacs.stackexchange.com/a/3661/115

(defvar modi/extend-rectangle--added-whitespace-boundaries '()
  "Alist to hold the (start . end) positions of the white space added
to the last line of the rectangle by `modi/extend-rectangle--core'.")

;; Below is inspired from the `rectangle-utils' package.
;; https://github.com/thierryvolpiatto/rectangle-utils
(defun modi/extend-rectangle--core (beg end)
  "For a rectangle selected between BEG and END, return an updated value of END
such that the longest line of region is completely included in the selection."
  (let* ((num-lines (count-lines beg end))
         (max-len 0)
         (line-info '())
         rect-left-col
         len)
    (setq modi/extend-rectangle--added-whitespace-boundaries (list end))
    ;; (message "Before: beg=%0d end=%0d" beg end)
    ;; When the end point of the region is on the 0th column, `count-lines'
    ;; returns a one less number, so increment that value by 1 to compensate.
    (when (save-excursion
            (goto-char end)
            (bolp))
      (setq num-lines (1+ num-lines)))
    (save-excursion
      (goto-char beg)
      (setq rect-left-col (current-column))
      ;; Calculate max-len
      (save-excursion
        (dotimes (i num-lines)
          (let ((line-end-col (save-excursion
                                (goto-char (line-end-position))
                                (current-column))))
            (setq len (- line-end-col rect-left-col))
            (add-to-list 'line-info `(,i ,line-end-col)))
          (when (> len max-len)
            (setq max-len len))
          (forward-line 1)))
      ;; Pad the last line with spaces if it's not the longest
      (let* ((last-line-num (1- num-lines))
             (line-end-col (nth 1 (assoc last-line-num line-info)))
             (len (- line-end-col rect-left-col))
             (diff (- max-len len)))
        (when (< len max-len)
          (forward-line last-line-num) ; go to the last line of the rectangle
          (goto-char (line-end-position))
          (let ((inhibit-read-only t)) ; ignore read-only status of buffer
            (insert (make-string diff ? )))
          (setq end (line-end-position))
          (setcdr modi/extend-rectangle--added-whitespace-boundaries (list end))))
      ;; (message "After: end=%0d" end)
      end)))

(defvar modi/extend-rectangle-fns '(extract-rectangle ; copy-rectangle-as-kill
                                    delete-extract-rectangle ; kill-rectangle
                                    delete-rectangle
                                    clear-rectangle
                                    modi/kill-rectangle-replace-with-space)
  "Functions where it would be useful to be able to extend the rectangle
selections to include the longest lines.")

(defun modi/advice-select-rectangle-to-end (orig-fun &rest args)
  "Extract the rectangle so that the longest line of region is completely
included when the prefix \\[universal-argument] is used.

In the below example, ▯ is the mark and ▮ is the point.

  a =▯12345;
  b = 6;▮

If that region is selected and if we do \\[copy-rectangle-as-kill], the following
rectangle gets copied:

  12
  6;

.. which was not the intention.

But with this advice, \\[universal-argument] \\[copy-rectangle-as-kill] on that
same region will copy the below rectangle:

  12345;
  6;

.. which obviously was the actual intention.

Similar rectangle extension behavior is applied when using the
prefix \\[universal-argument] with similarly advised functions too."
  (let (ret)
    (if (and (eq 4 (prefix-numeric-value current-prefix-arg)) ; C-u
             ;; We cannot activate this advise for `copy-rectangle-to-register'
             ;; because any prefix argument will enable its DELETE-FLAG arg. So
             ;; one way to make this work for `copy-rectangle-to-register' would
             ;; be to make the selection, call `modi/extend-rectangle-to-end'
             ;; and then call `copy-rectangle-to-register'.
             (not (eq this-command #'copy-rectangle-to-register)))
        (let* ((beg (nth 0 args))
               (end (nth 1 args)))
          ;; Override the original END argument
          (setcar (cdr args) (modi/extend-rectangle--core beg end))
          (setq ret (apply orig-fun args)))
      (setq ret (apply orig-fun args)))
    ;; Remove the extra whitespace added by `modi/extend-rectangle--core'
    (when (and (cdr modi/extend-rectangle--added-whitespace-boundaries)
               (or (eq this-command #'copy-rectangle-as-kill)
                   (eq this-command #'copy-rectangle-to-register)))
      (let ((inhibit-read-only t))
        (apply #'delete-trailing-whitespace
               modi/extend-rectangle--added-whitespace-boundaries)
        (setq modi/extend-rectangle--added-whitespace-boundaries nil)))
    ret))

(dolist (fn modi/extend-rectangle-fns)
  (advice-add fn :around #'modi/advice-select-rectangle-to-end)
  ;; (advice-remove fn #'modi/advice-select-rectangle-to-end)
  )

(defun modi/extend-rectangle-to-end (beg end)
  "Extend the rectangle based on the longest line of region."
  (interactive "r")
  (setq end (modi/extend-rectangle--core beg end))
  ;; Go back to BEG and push mark to new END.
  (goto-char beg)
  (push-mark end :nomsg :activate)
  (setq deactivate-mark nil))
(bind-key ">" #'modi/extend-rectangle-to-end region-bindings-mode-map)

(defun modi/kill-rectangle-replace-with-space (start end)
  "Kill the rectangle and replace it with spaces."
  (interactive "r")
  (setq killed-rectangle (extract-rectangle start end))
  (clear-rectangle start end)
  (setq deactivate-mark t)
  (if (called-interactively-p 'interactive)
      (indicate-copied-region (length (car killed-rectangle)))))

;; http://oremacs.com/2015/02/25/rectangle-hydra/
(defun ora-ex-point-mark ()
  (interactive)
  (if rectangle-mark-mode
      (exchange-point-and-mark)
    (let ((mk (mark)))
      (rectangle-mark-mode 1)
      (goto-char mk))))

(defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                           :color pink
                           :post (deactivate-mark)
                           :hint nil)
  "
  Rectangle:
  ^_p_^        _d_   delete      _s_tring        _c_/_C_ (delete/kill) and replace with space
_b_   _f_      _k_   cut         _r_eset         _o_pen (create blank rectangle and push text in region to the right)
  ^_n_^        _w_   copy        e_x_change      _X_ delete whitespace starting from the left edge of the rectangle
^^^^           _y_   paste       _e_xtend        Prefix rectangle lines with _N_umbers
"
  ("b"   backward-char)
  ("f"   forward-char)
  ("p"   previous-line)
  ("n"   next-line)

  ("d"   delete-rectangle :color blue)
  ("k"   kill-rectangle :color blue)
  ("w"   copy-rectangle-as-kill :color blue)
  ("y"   yank-rectangle :color blue)
  ("s"   string-rectangle :color blue)
  ("t"   string-rectangle :color blue)
  ("r"   (if (region-active-p)
             (deactivate-mark)
           (rectangle-mark-mode 1)))
  ("x"   ora-ex-point-mark)
  ("e"   modi/extend-rectangle-to-end)
  ("c"   clear-rectangle)
  ("C"   modi/kill-rectangle-replace-with-space :color blue)
  ("o"   open-rectangle :color blue)
  ("X"   delete-whitespace-rectangle :color blue)
  ("N"   rectangle-number-lines :color blue)
  ("q"   nil "cancel" :color blue))
(bind-key "C-x SPC" #'hydra-rectangle/body modi-mode-map)
(bind-key "r" #'hydra-rectangle/body region-bindings-mode-map)

;; Replace selection/rectangle with spaces
(bind-key "<S-SPC>" #'clear-rectangle region-bindings-mode-map)

(bind-keys
 :map modi-mode-map
 ;; Add an extra binding for `copy-rectangle-as-kill' as the default is a bit
 ;; inconvenient "C-x r M-w"
 ("C-x r e" . copy-rectangle-as-kill))

;;; Cycle Letter Case
;; http://ergoemacs.org/emacs/modernization_upcase-word.html
(defun xah-cycle-letter-case (arg)
  "Cycle the letter case of the selected region or the current word.
Cycles from 'lower' -> 'Capitalize' -> 'UPPER' -> 'lower' -> ..

        C-u M-x xah-cycle-letter-case -> Force convert to upper case.
    C-u C-u M-x xah-cycle-letter-case -> Force convert to lower case.
C-u C-u C-u M-x xah-cycle-letter-case -> Force capitalize."
  (interactive "p")
  (let (p1 p2
           (deactivate-mark nil)
           (case-fold-search nil))
    (if (use-region-p)
        (setq p1 (region-beginning)
              p2 (region-end))
      (let ((bds (bounds-of-thing-at-point 'word)))
        (setq p1 (car bds)
              p2 (cdr bds))))

    (cl-case arg
      (4  (put this-command 'next-state "UPPER"))      ; Force convert to upper case
      (16 (put this-command 'next-state "lower"))      ; Force convert to lower case
      (64 (put this-command 'next-state "Capitalize")) ; Force capitalize
      (t (when (not (eq last-command this-command))
           (save-excursion
             (goto-char p1)
             (cond
              ;; lower -> Capitalize
              ((looking-at "[[:lower:]]")            (put this-command 'next-state "Capitalize"))
              ;; Capitalize -> UPPER
              ((looking-at "[[:upper:]][[:lower:]]") (put this-command 'next-state "UPPER"))
              ;; Default: UPPER -> lower
              (t                                     (put this-command 'next-state "lower")))))))

    (cl-case (string-to-char (get this-command 'next-state)) ; `string-to-char' returns first character in string
      (?U (upcase-region p1 p2)
          ;; UPPER -> lower
          (put this-command 'next-state "lower"))
      (?l (downcase-region p1 p2)
          ;; lower -> Capitalize
          (put this-command 'next-state "Capitalize"))
      ;; Capitalization is a better option here than upcasing the initials
      ;; because (upcase-initials "abc") -> "Abc" (good)
      ;;         (upcase-initials "ABC") -> "ABC" (not what I expect most of the times)
      ;;         (capitalize "abc")      -> "Abc" (good)
      ;;         (capitalize "ABC")      -> "Abc" (good)
      (t (capitalize-region p1 p2)
         ;; Capitalize -> UPPER
         (put this-command 'next-state "UPPER")))))
(defun modi/upcase ()     (interactive) (xah-cycle-letter-case 4))
(defun modi/downcase ()   (interactive) (xah-cycle-letter-case 16))
(defun modi/capitalize () (interactive) (xah-cycle-letter-case 64))

(bind-keys
 :map region-bindings-mode-map
 ("~" . xah-cycle-letter-case))

(defhydra hydra-change-case (:color blue
                             :hint nil
                             :after-exit (when (region-active-p)
                                           (deactivate-mark)))
  "
_C_apitalize        _u_PCASE        _l_owercase        _<SPC>_ →Cap→UP→down→
"
  ("C"     modi/capitalize)
  ("c"     modi/capitalize)
  ("u"     modi/upcase)
  ("U"     modi/upcase)
  ("l"     modi/downcase)
  ("d"     modi/downcase)
  ("<SPC>" xah-cycle-letter-case :color red)
  ("M-c"   xah-cycle-letter-case :color red)
  ("q"     nil "cancel" :color blue))

(bind-keys
 :map modi-mode-map
 ("C-x C-u" . modi/upcase)
 ("C-x C-l" . modi/downcase)
 ("M-c"     . hydra-change-case/body))

;;; Sort Words
;; http://www.emacswiki.org/emacs/SortWords
;; http://emacs.stackexchange.com/a/7550/115
(defun sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
Prefixed with negative \\[universal-argument], sorts in reverse.

The variable `sort-fold-case' determines whether alphabetic case
affects the sort order. See `sort-regexp-fields'.

Temporarily consider - and _ characters as part of the word when sorting."
  (interactive "*P\nr")
  (let ((temp-table (copy-syntax-table text-mode-syntax-table)))
    (with-syntax-table temp-table
      (modify-syntax-entry ?- "w" temp-table)
      (modify-syntax-entry ?_ "w" temp-table)
      (sort-regexp-fields reverse "\\w+" "\\&" beg end))))

;;; Fill/unfill

(bind-key "f" #'fill-region region-bindings-mode-map)

;; par - http://www.nicemice.net/par/
;; http://emacs.stackexchange.com/a/26387/115
(when (executable-find "par")
  (defun modi/par-fill-region (begin end par-option)
    "Use `par' executable to fill region between BEGIN and END.

If PAR-OPTION is \\='(4) `\\[universal-argument]', also justify the text.
If PAR-OPTION is \\='(16) `\\[universal-argument] \\[universal-argument]', try to
make all lines of almost equal lengths instead of justifying.

See `man par' for more information."
    (interactive "r\nP")
    (let* ((debug-this nil)     ;Set to `t' temporarily when debugging
           (width-str (number-to-string fill-column))
           (err-buf "*par Error*")
           (par-cmd
            (concat "par "
                    "w" width-str " "   ;Set fill width
                    ;; Body characters: . , ? upper-case lower-case
                    (shell-quote-argument "B=.,?_A_a") " "
                    ;; Quote characters: space > | <comment-start char>
                    (shell-quote-argument
                     (concat "Q=_s>|" (when comment-start comment-start))) " "
                    ;; Allow bodiless characters like *, space, .. to inc/dec by
                    ;; up to 10 characters to adjust line width.
                    "r10"
                    "T4"                ;Expand tab chars to 4 spaces
                    ;; Prefixes may not contain any trailing body characters, and
                    ;; suffixes may not contain any leading body characters
                    "b"
                    "e"                ;Expel/remove superfluous lines
                    (when (equal '(16) par-option)
                      "f") ;Try to make all lines of nearly the same length
                    "g"  ;Make a better guess at inserting line breaks
                    (when (equal '(4) par-option)
                      "j") ;Justify the lines by inserting spaces between words
                    "q"   ;Insert blank lines before/after quoted text
                    "R" ;Throw an error if a word length exceeds the fill width
                    "E" ))     ;Send error to stderr instead of stdout
           (before-text (buffer-substring-no-properties begin end))
           par-ret
           ;; Do the formatting in a temp buffer so that the text in the original
           ;; buffer doesn't get corrupted in case `par' fails due to some error.
           (after-text (with-temp-buffer
                         (insert before-text)
                         (unless debug-this
                           (setq par-ret (shell-command-on-region
                                          (point-min) (point-max)
                                          par-cmd nil :replace
                                          err-buf :display-error-buffer)))
                         (buffer-substring-no-properties (point-min) (point-max)))))
      (message "Executing `%s' on the region .." par-cmd)
      (unless debug-this
        (cond
         ;; If 1 is returned, error occurred in the cmd execution; 0 - no error
         ((= 1 par-ret)
          ;; Switch to the error buffer
          (switch-to-buffer-other-window err-buf)
          (special-mode)) ;Set this mode so that you can quit it quickly using C-u q
         ((= 0 par-ret)
          ;; If no error occurred, do below in the original buffer
          (delete-region begin end)
          (insert after-text))))))
  (bind-key "F" #'modi/par-fill-region region-bindings-mode-map)
  ;; Rationale for the below binding is that it flows very well when selecting a
  ;; paragraph and then filling that region: "M-h M-H"
  (bind-key "M-H" #'modi/par-fill-region modi-mode-map))

;; Forked version of https://github.com/purcell/unfill
(use-package unfill
  :load-path "elisp/unfill")

;;; Replace identical strings with incremental number suffixes
(defvar modi/rwins-max 100
  "Default maximum number of replacements.")

(defvar modi/rwins-incr 1
  "Default number by which the number suffixes will increment in the
replacements.")

(defun modi/replace-with-incr-num-suffix (start)
  "Replace selected region/symbol at point with incrementing number suffixes.

If START is non-nil, the replacements will be suffixes with the START number
and increment by 1 on each replacement.

If START is nil and if the selected region or symbol already ends in a number,
the replacements will use that number as the START number.

If START is nil and if the selected region or symbol does NOT end in a number,
the replacements will use 1 as the START number.

`modi/rwins-max' controls the maximum number till which the suffix number
increments. After the max number is reached, the suffixes will restart from
START (behavior of `map-query-replace-regexp').

`modi/rwins-incr' controls the increments between the number suffixes in
consecutive replacements.

  Example:
  Initial text:
     Here3 Here3 Here3 Here3 Here3
  After replacement text:
     Here3 Here4 Here5 Here6 Here7

Note that the selected region cannot contain any spaces."
  (interactive "p")
  (let (raw-str beg non-number-str to-strings)
    (cond ((use-region-p)
           (setq raw-str (buffer-substring-no-properties
                          (region-beginning) (region-end)))
           (setq beg (region-beginning)))
          ((symbol-at-point)
           (setq raw-str (substring-no-properties
                          (symbol-name (symbol-at-point))))
           (setq beg (car (bounds-of-thing-at-point 'symbol)))))
    (if (string-match "\\b\\(\\w*?\\)\\([0-9]+\\)$" raw-str)
        (progn
          (setq non-number-str (match-string-no-properties 1 raw-str))
          (when (null current-prefix-arg)
            (setq start (string-to-number (match-string-no-properties 2 raw-str)))))
      (setq non-number-str raw-str))
    ;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Mapping-Functions.html
    (setq to-strings (mapconcat (lambda (x) (concat non-number-str (number-to-string x)))
                                (number-sequence
                                 start
                                 (+ start (* modi/rwins-incr (1- modi/rwins-max)))
                                 modi/rwins-incr)
                                " "))
    (goto-char beg) ; Go to the start of the selection/symbol
    (map-query-replace-regexp (regexp-quote raw-str) to-strings)))

;;; Delete Blank Lines
;; http://www.masteringemacs.org/article/removing-blank-lines-buffer
;; If a region is selected, delete all blank lines in that region.
;; Else, call `delete-blank-lines'.
(defun modi/delete-blank-lines-in-region (&rest args)
  (let ((do-not-run-orig-fn (use-region-p)))
    (when do-not-run-orig-fn
      (flush-lines "^[[:blank:]]*$" (region-beginning) (region-end)))
    do-not-run-orig-fn))
(advice-add 'delete-blank-lines :before-until #'modi/delete-blank-lines-in-region)

;;; Space Adjustment After Word Kills
(defun modi/just-one-space-post-kill-word (&rest _)
  "Function to manage white space after `kill-word' operations.

1. If point is at the beginning of the line after possibly some white space,
   remove that white space and re-indent that line.
2. If there is space before or after the point, ensure that there is only
   one white space around the point.
3. Otherwise, do nothing.

During the whole operation do not change the point position with respect to the
surrounding white space.

abc|   def  ghi <-- point on the left of white space after 'abc'
abc| ghi        <-- point still before white space after calling this function
abc   |def  ghi <-- point on the right of white space before 'def'
abc |ghi        <-- point still after white space after calling this function."
  (cond ((looking-back "^ *") ; remove extra space at beginning of line
         (save-excursion ; maintain the initial position of the pt w.r.t. space
           (just-one-space 0))
         (indent-according-to-mode))
        ((or (looking-at   " ")
             (looking-back " ")) ; adjust space only if it exists
         (save-excursion ; maintain the initial position of the pt w.r.t. space
           (just-one-space 1)))
        (t ; do nothing otherwise, includes the case where the point is at EOL
         )))
;; Delete extra horizontal white space after `kill-word' and `backward-kill-word'
(advice-add 'kill-word :after #'modi/just-one-space-post-kill-word)
;; (advice-remove 'kill-word #'modi/just-one-space-post-kill-word)

;;; Operate on Region or Whole Buffer
(defvar modi/region-or-whole-fns '(indent-region
                                   eval-region)
  "List of functions to act on the whole buffer if no region is selected.")

(defun modi/advice-region-or-whole (orig-fun &rest args)
  "Advice function that applies ORIG-FUN to the whole buffer if no region is
selected.
http://thread.gmane.org/gmane.emacs.help/109025/focus=109102 "
  ;; Required to override the "r" argument of `interactive' in functions like
  ;; `indent-region' so that they can be called without an active region.
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))
  ;; (message "Args: %S R: %S I: %S"
  ;;          args (use-region-p) (called-interactively-p 'interactive))
  (prog1 ; Return value of the advising fn needs to be the same as ORIG-FUN
      (apply orig-fun args)
    (when (and (called-interactively-p 'interactive)
               (not (use-region-p)))
      (message "Executed %s on the whole buffer."
               (propertize (symbol-name this-command)
                           'face 'font-lock-function-name-face)))))

(dolist (fn modi/region-or-whole-fns)
  (advice-add fn :around #'modi/advice-region-or-whole)
  ;; (advice-remove fn #'modi/advice-region-or-whole)
  )

;;; Mark Management

;;;; Popping marks
;; http://endlessparentheses.com/faster-pop-to-mark-command.html
;; https://github.com/magnars/expand-region.el/issues/159#issuecomment-83538021
(defun modi/multi-pop-to-mark (orig-fun &rest args)
  "When popping the mark, continue popping until the cursor actually moves.
Try the repeated popping up to 10 times."
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point))
        (apply orig-fun args)))))
(advice-add 'pop-to-mark-command :around #'modi/multi-pop-to-mark)

;; Ensure that we can quickly pop the mark several times by typing
;; C-u C-SPC C-SPC, instead of having to type C-u C-SPC C-u C-SPC.
(setq set-mark-command-repeat-pop t)

;;;; Smart Mark
;; C-g after `mark-whole-buffer' brings the point back to where it originally was.
(use-package smart-mark
  :config
  (progn
    (smart-mark-mode 1)))

;;; Tweaking `region-extract-function'
(defun modi/region-extract-function--C-u-kill (orig delete)
  "When a region is selected,
and if \\[universal-argument] is used,
and DELETE is \\='delete (when doing \\[kill-region]), or
    DELETE is nil (when doing \\[kill-ring-save]),
kill the region with all trailing whitespace removed and also replace 2
or more spaces with single spaces.
Else, execute ORIG function."
  (if (and (region-beginning)
           (eq 4 (prefix-numeric-value current-prefix-arg)) ; when using C-u, and
           (or (eq delete 'delete) ; when cutting (C-w), or
               (eq delete nil))) ; when copying (M-w)
      (let ((sel (filter-buffer-substring (region-beginning) (region-end) delete)))
        (with-temp-buffer
          (insert sel)
          (delete-trailing-whitespace)
          (goto-char (point-min))
          (while (re-search-forward "\\s-\\{2,\\}" nil :noerror)
            (replace-match " "))
          (buffer-string)))
    (funcall orig delete)))
(add-function :around region-extract-function #'modi/region-extract-function--C-u-kill)

;;; Commenting
;; http://stackoverflow.com/questions/9688748/emacs-comment-uncomment-current-line
;; http://endlessparentheses.com/implementing-comment-line.html
(defun endless/comment-line-or-region (n)
  "Comment or uncomment current line and proceed to the next line.
 With positive prefix, apply to N lines including current one.
 With negative prefix, apply to -N lines above.
 If region is active, apply to active region instead."
  (interactive "p")
  (if (use-region-p)
      (comment-or-uncomment-region
       (region-beginning) (region-end))
    (let ((range
           (list (line-beginning-position)
                 (goto-char (line-end-position n)))))
      (comment-or-uncomment-region
       (apply #'min range)
       (apply #'max range)))
    (forward-line 1)
    (back-to-indentation)))

;;; Anonymize
(defun modi/anonymize (&optional only-numbers)
  "Replace alphabetical and numerical characters with random lowercase alphabets.

Anonymize the selected region. If no region is selected, apply this function on
the whole buffer.

This function is useful when you want share an anonymized code snippet to someone
to help with some debug.

If ONLY-NUMBERS is non-nil, randomize only the numbers."
  (interactive "P")
  (let ((beg (if (use-region-p) (region-beginning) (point-min)))
        (end (if (use-region-p) (region-end) (point-max))))
    (save-restriction
      (narrow-to-region beg end)
      (save-excursion
        (let ((case-fold-search nil))
          (unless only-numbers
            (goto-char (point-min))
            (while (re-search-forward "[a-z]" nil :noerror)
              (replace-match (char-to-string (+ ?a (random (- ?z ?a))))))
            (goto-char (point-min))
            (while (re-search-forward "[A-Z]" nil :noerror)
              (replace-match (char-to-string (+ ?A (random (- ?Z ?A)))))))
          (goto-char (point-min))
          (while (re-search-forward "[0-9]" nil :noerror)
            (replace-match (char-to-string (+ ?0 (random (- ?9 ?0)))))))))))
(defalias 'modi/obfuscate-text 'modi/anonymize)
(defalias 'modi/randomize-text 'modi/anonymize)

(defun modi/randomize-numbers ()
  "Randomize numbers in the selected region or the whole buffer."
  (interactive)
  (modi/anonymize :only-numbers))

;;; Keep Lines - Because I Said So
(defun modi/keep-lines-force (regexp)
  "Do `keep-lines' even in read-only buffers.

Useful for quickly filtering lines to show only the ones matching
REGEXP in read-only buffers in eww, package manager, etc.

Once done reviewing the filtered results, hitting \"g\" in the
buffer should do the right thing.. `eww-reload' in eww,
`revert-buffer' in package manager."
  (interactive (list (read-from-minibuffer
                      "Keep only lines matching regexp: ")))
  (let ((inhibit-read-only t))          ;Ignore read-only status
    (save-excursion
      (goto-char (point-min))
      (keep-lines regexp))))

(defun modi/search-replace-pairs (sr-pairs)
  "Search/replace in the buffer/region using SR-PAIRS.
SR-PAIRS is a list of cons (SEARCH-REGEX . REPLACE-EXPR) where
the cons elements are strings."
  (let ((cnt 0)
        (beg (if (use-region-p) (region-beginning) (point-min)))
        (end (if (use-region-p) (region-end) (point-max))))
    (dolist (pair sr-pairs)
      (let ((search-regex (car pair))
            (replace-expr (cdr pair)))
        (save-restriction
          (narrow-to-region beg end)
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward search-regex nil :noerror)
              (replace-match replace-expr)
              (cl-incf cnt))))))
    (message "Finished %d replacements" cnt)))
;; Example use: M-: (modi/search-replace-pairs '(("a" . "b")))

;;; Bindings

;; After indenting rigidly interactively, hitting Enter should copy the indented
;; selection (and quit the interactive rigid indentation mode).
(bind-key "RET" #'kill-ring-save indent-rigidly-map)

(bind-keys
 :map modi-mode-map
 ("C-x d" . delete-region)
 ("C-S-d" . duplicate-current-line-or-region)
 ("C-M-d" . duplicate-current-line-or-region) ;Alternative to C-S-d in terminal mode, overrides `down-list'
 ("C-x \\" . align-regexp) ; align selected region to the entered regexp
 ;; Align multiple columns in the selected region. Of course all the selected
 ;; lines must have the same number of columns of groups of non-space characters
 ("C-x |" . modi/align-columns)
 ("C-k" . modi/kill-line)
 ("C-o" . modi/smart-open-line)        ;Overrides `open-line'
 ("C-j" . modi/pull-up-line)
 ("M-=" . count-words) ; count words in buffer if no region selected
 ("M-;" . endless/comment-line-or-region) ;Overrides`comment-dwim'
 ("C-x ;" . comment-dwim)                 ;Overrides `comment-set-column'
 ;; Override M-backspace to always do `backward-kill-word' using `modi-mode-map'.
 ;; Below is required so that `verilog-mode' does not bind it to `kill-word'.
 ("<M-delete>" . backward-kill-word)
 ("<C-M-backspace>" . backward-kill-sexp))
(bind-to-modi-map "=" #'modi/align-to-equals)


(provide 'setup-editing)

;; (1) Commenting
;; |-------------+-----------------------------------------------------------|
;; | Binding     | Description                                               |
;; |-------------+-----------------------------------------------------------|
;; | M-j / C-M-j | `indent-new-comment-line' (default)                       |
;; |             | This creates a commented new line; useful when writing    |
;; |             | multiline comments like this one without having to        |
;; |             | manually type in the comment characters.                  |
;; | C-x C-;     | `comment-line' (default) [new in emacs 25.1]              |
;; |             | Comments line or region and moves point to the next line. |
;; | M-;         | `comment-dwim' (default)                                  |
;; |-------------+-----------------------------------------------------------|
;;
;; (2) Undo
;; `C-_' or `C-/'
;;
;; (3) Toggle read-only-mode; toggles buffers between editable and read-only states.
;; `C-x C-q' or `M-x read-only-mode'
;;
;; (4) Backups using prefix args to `save-buffer' (C-x C-s)
;;                 C-x C-s -> Makes the prev version into a backup file if
;;                            previously requested or if this is the first save.
;;             C-u C-x C-s -> Marks this version to become a backup when the
;;                            next save is done.
;;         C-u C-u C-x C-s -> Unconditionally makes the previous version into
;;                            a backup file.
;;     C-u C-u C-u C-x C-s -> Does both of what C-u prefix and C-u C-u prefix do.
;;
;; (5) The new `comment-line' function in emacs 25.1 is based off the above
;;     `endless/comment-line-or-region' function. But I do not like `comment-line'
;;     as it always comments/uncomments *whole* lines only, even when a region is
;;     selected.
;;
;;     Annoyances of `comment-line':
;;
;;     - Case where the `mark' is at the BOL. I do not intend to comment the 3rd
;;       line in the below example.
;;
;;       ▮(let ((foo "bar"))                        ;; (let ((foo "bar"))
;;         (message foo))      -- comment-line -->  ;;   (message foo))
;;       ▯(let ((baz "cat"))                        ;; (let ((baz "cat"))
;;         (message baz))                                (message baz))
;;
;;     - Case where I intend to comment out *only* the selected sexp, not the whole
;;       line.
;;
;;       (let (▮(foo "bar")▯)  -- comment-line -->  ;; (let ((foo "bar"))
;;         (message foo))                             (message foo))
;;
;;     Above gets fixed by `endless/comment-line-or-region':
;;
;;       ▮(let ((foo "bar"))                                          ;; (let ((foo "bar"))
;;         (message foo))      -- endless/comment-line-or-region -->  ;;   (message foo))
;;       ▯(let ((baz "cat"))                                          (let ((baz "cat"))
;;         (message baz))                                                  (message baz))
;;
;;       (let (▮(foo "bar")▯)  -- endless/comment-line-or-region -->  (let (;; (foo "bar")
;;         (message foo))                                                   )
;;                                                                      (message foo))
;;
;; (6) Selecting a region and hitting C-u C-x TAB will indent that selection to
;;     the right by 4 spaces. See `indent-rigidly'.
