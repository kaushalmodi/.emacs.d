;; Time-stamp: <2015-08-21 13:55:48 kmodi>

;; Functions related to editing text in the buffer

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

;; Duplicate current line or region
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

;; Managing white spaces and empty newlines
;; Delete trailing white space in lines and empty new lines at the end of file
;; at the time of saving
;; This is very useful for macro definitions in Verilog as for multi-line
;; macros, NO space is allowed after line continuation character "\"
(defvar do-not-delete-trailing-whitespace nil
  "If nil, `delete-trailing-whitespace' function will be executed in the
`write-file-functions' hook.

Usage: If you do not want to automatically delete the trailing whitespace in
any of the files in a given sub-directory, create a `.dir-locals.el' file in
that sub-directory will the below contents:
  ((\"sub-dir-name\"
    . ((nil . ((do-not-delete-trailing-whitespace . t))))))
")
(make-variable-buffer-local 'do-not-delete-trailing-whitespace)
(defun modi/delete-trailing-whitespace ()
  "Call the `delete-trailing-whitespace' function only if the
`do-not-delete-trailing-whitespace' variable is `nil' or unbound."
  (when (not (bound-and-true-p do-not-delete-trailing-whitespace))
    (call-interactively #'delete-trailing-whitespace)))
(add-hook 'write-file-functions #'modi/delete-trailing-whitespace)

;; Untabify buffer
(defun modi/untabify-buffer ()
  "Untabify the current buffer."
  (interactive)
  (untabify (point-min) (point-max))
  ;; http://www.veripool.org/issues/345-Verilog-mode-can-t-get-untabify-on-save-to-work
  ;; Note that the function's return value is set to `nil' because if this
  ;; function is added to `write-file-functions' hook, emacs will stay stuck at
  ;; at the "Saving file .." message and the file won't be saved (as one of the
  ;; functions in the `write-file-functions' is not returning `nil'.
  nil)

;; Align
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
  (align-regexp begin end "\\(\\s-*\\)=" 1 1 nil))
;; To do it manually do `M-x align-regexp`, type `=` and hit Enter

;; To perform align-regexp WITHOUT the default values of regexp, group, spacing, repeat
;; do `C-u M-x align-regexp`

(defun modi/align-columns (begin end)
  "Align text columns"
  (interactive "r")
  ;; align-regexp syntax:  align-regexp (beg end regexp &optional group spacing repeat)
  (align-regexp begin end "\\(\\s-+\\)[a-zA-Z0-9=(),?':`\.{}]" 1 1 t)
  (indent-region begin end)) ; indent the region correctly after alignment

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

;; Toggle comment on current line or selected region
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

;; http://emacs.stackexchange.com/q/7519/115
(defun modi/pull-up-line ()
  "Join the following line onto the current one (analogous to `C-e', `C-d') or
`C-u M-^' or `C-u M-x join-line'.

If the current line is a comment and the pulled-up line is also a comment,
remove the comment characters from that line."
  (interactive)
  (join-line -1)
  ;; If the current line is a comment
  (when (nth 4 (syntax-ppss))
    ;; Remove the comment prefix chars from the pulled-up line if present
    (save-excursion
      ;; Delete all comment-start and space characters
      (while (looking-at (concat "\\s<" ; comment-start char as per syntax table
                                 "\\|" (substring comment-start 0 1) ; first char of `comment-start'
                                 "\\|" "\\s-")) ; extra spaces
        (delete-forward-char 1))
      (insert-char ? ) ; insert space
      )))

;; Enable narrowing
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)

;; Enable setting goal column
;; http://emacsblog.org/2007/03/17/quick-tip-set-goal-column/
(put 'set-goal-column  'disabled nil)
;;     C-x C-n <- Set goal column
;; C-u C-x C-n <- Unset goal column

;; Enable conversion of the selected region to upper case using `C-x C-u`
(put 'upcase-region   'disabled nil)
;; Enable conversion of the selected region to lower case using `C-x C-l`
(put 'downcase-region 'disabled nil)

;; zop-to-char
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

;; http://stackoverflow.com/q/12165205/1219634
(defun kill-with-linenum (beg end unicode)
  "Copy the selected region with file name, starting and ending
line numbers, date and user name.

When called with a prefix like `C-u', it will use unicode characters
instead of ASCII characters for adorning the copied snippet."
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
          (buffer-or-file-name (or (buffer-file-name) (buffer-name)))
          (user (getenv "USER")))
      (setq chunk (concat
                   (format "%s #%-d %s %s %s\n%s "
                           chars-start-of-code-block
                           (line-number-at-pos beg)
                           chars-n-dash
                           (replace-regexp-in-string
                            (concat "_" user) "_${USER}" buffer-or-file-name)
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
                           user)))
      (kill-new chunk)))
  (deactivate-mark))

(defun kill-with-linenum-unicode (beg end)
  (interactive "r")
  (kill-with-linenum beg end :unicode))

(when (featurep 'region-bindings-mode)
  (bind-keys
   :map region-bindings-mode-map
    ;; When region is selected, pressing `c' will copy the region
    ;; with ASCII character adornment.
    ;; Pressing `C-u c' or `C' will copy with Unicode character adornment.
    ("c" . kill-with-linenum)
    ("C" . kill-with-linenum-unicode)))

;; Convert the decimal values in the whole buffer to 16-bit 2's complement hex
(fset 'modi/convert-dec-to-twos-comp-16-bit-hex
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([escape 58 40 114 101 97 100 45 111 110 108 121 45 109 111 100 101 32 45 49 41 return 109 44 19 45 return 67108896 2 97 67108896 40 6 32 40 94 32 50 32 49 54 41 32 5 41 24 109 120 return 24 104 33554435 33554435 40 102 111 114 109 97 116 32 34 37 88 34 5 41 24 109 120 return] 0 "%d")) arg)))

;; RECTANGLE

;; How to position the cursor after the end of line; useful for copying/killing
;; rectangles have lines of varying lengths.
;; http://emacs.stackexchange.com/a/3661/115
(use-package rectangle-utils
  ;; :load-path "elisp/rectangle-utils"
  :commands (extend-rectangle-to-end)
  :init
  (progn
    (when (featurep 'region-bindings-mode)
      (bind-keys
       :map region-bindings-mode-map
        ("|" . extend-rectangle-to-end)))))

(defun copy-rectangle-as-kill-then-delete (start end)
  "Copy the region-rectangle and save it as the last killed one.
Then delete the rectangle, which will replaced the deleted region with blank
spaces."
  (interactive "r")
  (setq killed-rectangle (extract-rectangle start end))
  (delete-rectangle start end)
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
                           :post (deactivate-mark))
  "

  ^_p_^     _d_   delete   _s_tring
_b_   _f_   _k_   cut      _r_eset
  ^_n_^     _w_   copy     e_x_change
^^^^        _y_   paste    _e_xtend
"
  ("b"   backward-char                      nil)
  ("f"   forward-char                       nil)
  ("p"   previous-line                      nil)
  ("n"   next-line                          nil)
  ("w"   copy-rectangle-as-kill             nil)
  ("k"   kill-rectangle                     nil)
  ("K"   copy-rectangle-as-kill-then-delete nil)
  ("y"   yank-rectangle                     nil)
  ("d"   delete-rectangle                   nil)
  ("s"   string-rectangle                   nil)
  ("t"   string-rectangle                   nil)
  ("x"   ora-ex-point-mark                  nil)
  ("r"   (if (region-active-p)
             (deactivate-mark)
           (rectangle-mark-mode 1))         nil)
  ("e"   modi/extend-rectangle-to-end       nil)
  ("q"   nil                                nil))
(bind-key "C-x SPC" #'hydra-rectangle/body modi-mode-map)

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
      (t (upcase-initials-region p1 p2)
         ;; Capitalize -> UPPER
         (put this-command 'next-state "UPPER")))))
(defun modi/upcase ()     (interactive) (xah-cycle-letter-case 4))
(defun modi/downcase ()   (interactive) (xah-cycle-letter-case 16))
(defun modi/capitalize () (interactive) (xah-cycle-letter-case 64))

(when (featurep 'region-bindings-mode)
  (bind-keys
   :map region-bindings-mode-map
    ("~" . xah-cycle-letter-case)))

(defhydra hydra-change-case (:color blue
                             :hint nil)
  "
_C_apitalize        _U_PCASE        _d_owncase        _<SPC>_ →Cap→UP→down→
"
  ("C"     modi/capitalize)
  ("c"     modi/capitalize)
  ("U"     modi/upcase)
  ("u"     modi/upcase)
  ("d"     modi/downcase)
  ("l"     modi/downcase)
  ("<SPC>" xah-cycle-letter-case :color red)
  ("M-c"   xah-cycle-letter-case :color red)
  ("q"     nil "cancel" :color blue))

(bind-keys
 :map modi-mode-map
  ("C-x C-u" . modi/upcase)
  ("C-x C-l" . modi/downcase)
  ("M-c"     . hydra-change-case/body))

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

;; Forked version of https://github.com/purcell/unfill
(use-package unfill
  :load-path "elisp/unfill")

;; Copy region with formatting for G+ comments
;; https://github.com/jorgenschaefer/gplusify
(use-package gplusify
  :commands (gplusify-region-as-kill)
  :init
  (progn
    (when (featurep 'region-bindings-mode)
      (bind-keys
       :map region-bindings-mode-map
        ("G" . gplusify-region-as-kill)))))

;; Insert date, time, user name
(defun modi/insert-time-stamp (option)
  "Insert date, time, user name - DWIM.

If the point is NOT in a comment/string, the time stamp is inserted prefixed
with `comment-start' characters.

If the point is IN a comment/string, the time stamp is inserted without the
`comment-start' characters. If the time stamp is not being inserted immediately
after the `comment-start' characters (followed by optional space),
the time stamp is inserted with “--” prefix.

If the buffer is in a major mode where `comment-start' var is nil, no prefix is
added regardless.

Additional control:

        C-u -> Only `comment-start'/`--' prefixes are NOT inserted
    C-u C-u -> Only user name is NOT inserted
C-u C-u C-u -> Both prefix and user name are not inserted."
  (interactive "P")
  (let ((current-date-time-format "%a %b %d %H:%M:%S %Z %Y"))
    ;; Insert a space if there is no space to the left of the current point
    ;; and it's not at the beginning of a line
    (when (and (not (looking-back "^ *"))
               (not (looking-back " ")))
      (insert " "))
    ;; Insert prefix only if `comment-start' is defined for the major mode
    (when (stringp comment-start)
      (if (or (nth 3 (syntax-ppss)) ; string
              (nth 4 (syntax-ppss))) ; comment
          ;; If the point is already in a comment/string
          (progn
            ;; If the point is not immediately after `comment-start' chars
            ;; (followed by optional space)
            (when (and (not (or (equal option '(4)) ; C-u or C-u C-u C-u
                                (equal option '(64))))
                       (not (looking-back (concat comment-start " *")))
                       (not (looking-back "^ *")))
              (insert "--")))
        ;; If the point is NOT in a comment
        (progn
          (when (not (or (equal option '(4)) ; C-u or C-u C-u C-u
                         (equal option '(64))))
            (insert comment-start)))))
    ;; Insert a space if there is no space to the left of the current point
    ;; and it's not at the beginning of a line
    (when (and (not (looking-back "^ *"))
               (not (looking-back " ")))
      (insert " "))
    (insert (format-time-string current-date-time-format (current-time)))
    (when (not (or (equal option '(16)) ; C-u C-u or C-u C-u C-u
                   (equal option '(64))))
      (insert (concat " - " (getenv "USER"))))
    ;; Insert a space after the time stamp if not at the end of the line
    (when (not (looking-at " *$"))
      (insert " "))))
(bind-key "C-c D" #'modi/insert-time-stamp modi-mode-map)

;; Replace identical strings with incremental number suffixes
(defvar modi/replace-with-incr-num-suffix-max 100
  "Default maximum number till which the number suffixes will increment in the
replacements.")

(defvar modi/replace-with-incr-num-suffix-incr 1
  "Default number by which the number suffixes will increment in the
replacements.")

(defun modi/replace-with-incr-num-suffix (start)
  "Replace all instances of the selected region or the symbol under point with
the same, but suffixed with an incrementing number.

If START is non-nil, the replacements will be suffixes with the START number
and increment by 1 on each replacement.

If START is nil and if the selected region or symbol already ends in a number,
the replacements will use that number as the START number.

If START is nil and if the selected region or symbol does NOT end in a number,
the replacements will use 1 as the START number.

`modi/replace-with-incr-num-suffix-max' variable controls the maximum number
till which the suffix number increments. After the max number is reached, the
suffixes will restart from START (behavior of `map-query-replace-regexp').

`modi/replace-with-incr-num-suffix-incr' variable controls the increments
between the number suffixes in consecutive replacements.

Example:
Initial text:
   Here3 Here3 Here3 Here3 Here3
After replacement text:
   Here3 Here4 Here5 Here6 Here7
"
  (interactive "p")
  (let ((regexp (cond ((use-region-p)
                       (buffer-substring-no-properties (region-beginning)
                                                       (region-end)))
                      ((symbol-at-point)
                       (substring-no-properties
                        (symbol-name (symbol-at-point))))))
        prefix
        to-strings)
    (if (string-match "\\b\\(\\w*?\\)\\([0-9]+\\)$" regexp)
        (progn
          (setq prefix (match-string-no-properties 1 regexp))
          (when (null current-prefix-arg)
            (setq start (string-to-number (match-string-no-properties 2 regexp)))))
      (progn
        (setq prefix regexp)))
    ;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Mapping-Functions.html
    (setq to-strings (mapconcat (lambda (x) (concat prefix (number-to-string x)))
                                (number-sequence start
                                                 modi/replace-with-incr-num-suffix-max
                                                 modi/replace-with-incr-num-suffix-incr)
                                " "))
    ;; Go to the start of the current word
    (when (not (looking-back "\\b"))
      (re-search-backward "\\b" nil :noerror))
    (map-query-replace-regexp regexp to-strings)))

;; Backups

;; When `vc-make-backup-files' is nil (default), backups are not made for
;; version controlled (e.g. git) files. Set this to `t' to allow making backups
;; using prefix args to `save-buffer' command.
(setq vc-make-backup-files t)

;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Numbered-Backups.html
;; http://stackoverflow.com/a/151946/1219634
;; `version-control' is nil by default; numbered backups are made only if the
;; the visited file already has numbered backups. Below will always create
;; numbered backups.
(setq version-control :make-numbered-backups)

(setq kept-new-versions 4) ; default 2
(setq kept-old-versions 2) ; default 2
;; If there are backups numbered 1, 2, 3, 5, and 7, and both of the above
;; variables have the value 2, then the backups numbered 1 and 2 are kept
;; as old versions and those numbered 5 and 7 are kept as new versions;
;; backup version 3 is deleted after user confirmation.

;; Excess backup deletion will happen silently, without user confirmation, if
;; `delete-old-versions' is set to `t'.
(setq delete-old-versions t) ; default nil

;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Rename-or-Copy.html
(setq backup-by-copying t) ; don't clobber symlinks

(setq modi/backup-directory
      (let ((dir (concat temporary-file-directory
                         (getenv "USER") "/.backups/"))) ; must end with /
        (make-directory dir :parents)
        dir))

;; Save all backups to `modi/backup-directory'
(setq backup-directory-alist `(("." . ,modi/backup-directory)))
(message (format "All backup files will be saved to %s." modi/backup-directory))

;; http://ergoemacs.org/emacs/elisp_make-backup.html
(defun modi/make-backup ()
  "Make a backup copy of current file.
The backup file name has the form ‹name›~‹timestamp›~, in the same dir.
If such a file already exist, it's overwritten.
If the current buffer is not associated with a file, nothing's done."
  (interactive)
  (if (buffer-file-name)
      (let* ((currentName (buffer-file-name))
             (backupName (concat currentName
                                 "." (format-time-string "%Y%m%d_%H%M") ".bkp")))
        (copy-file currentName backupName :overwrite-if-already-exists)
        (message (concat "Backup saved as: " (file-name-nondirectory backupName))))
    (user-error "buffer is not a file.")))
;; Also make emacs ignore that appended string to the backup files when
;; deciding the major mode
;; http://emacs.stackexchange.com/a/13285/115
(add-to-list 'auto-mode-alist '("\\.[0-9_]+\\.bkp\\'" nil backup-file))
(bind-to-modi-map "`" #'modi/make-backup)

;; Unicode
(require 'iso-transl)
;; Add custom bindings to "C-x 8" map
(dolist (binding '(;; >
                   (">"       . nil) ; First unbind ">" from the map
                   (">="      . [?≥]) ; greater than or equal to
                   (">>"      . [?≫]) ; much greater than
                   (">\""     . [?»]) ; right-pointing double angle quotation mark
                   (">'"      . [?›]) ; single right-pointing angle quotation mark
                   (">h"      . [?☛]) ; black right pointing index
                   ;; <
                   ("<"       . nil) ; First unbind "<" from the map
                   ("<="      . [?≤]) ; less than or equal to
                   ("<<"      . [?≪]) ; much less than
                   ("<\""     . [?«]) ; left-pointing double angle quotation mark
                   ("<'"      . [?‹]) ; single left-pointing angle quotation mark
                   ("<h"      . [?☚]) ; black left pointing index
                   ;; "
                   ("\"`"     . [?“]) ; left double quotation mark
                   ("\"'"     . [?”]) ; right double quotation mark
                   ;; arrows
                   ("<right>" . [?→]) ; rightwards arrow
                   ("<left>"  . [?←]) ; leftwards arrow
                   ("<up>"    . [?↑]) ; upwards arrow
                   ("<down>"  . [?↓]) ; downwards arrow
                   ;; misc
                   ("r"       . [?▯]) ; white vertical rectangle
                   ("R"       . [?▮]) ; black vertical rectangle
                   ("*r"      . [?₹]) ; indian rupee sign
                   ("e"       . [?↵]) ; downwards arrow with corner leftwards
                   ("E"       . [?⏎]) ; return symbol
                   ("1/3"     . [?⅓]) ; fraction one third
                   ("0"       . [?​]))) ; zero width space
  (define-key iso-transl-ctl-x-8-map (kbd (car binding)) (cdr binding)))
;; Unicode chars that can be entered using C-x 8 binding
;; |------------------+-------------------------|
;; | C-x 8 prefix map | Unicode                 |
;; | binding          | character               |
;; |------------------+-------------------------|
;; | {                | “                       |
;; | }                | ”                       |
;; | ^1               | ¹                       |
;; | ^2               | ²                       |
;; | ^3               | ³                       |
;; | .                | ·                       |
;; | **               | •                       |
;; | o                | ° (degree)              |
;; | ~=               | ≈                       |
;; | /=               | ≠                       |
;; | +                | ±                       |
;; | /o               | ø                       |
;; | 1/2              | ½                       |
;; | 1/4              | ¼                       |
;; | 3/4              | ¾                       |
;; | R                | ®                       |
;; | C                | ©                       |
;; | m                | µ                       |
;; | 'a               | á                       |
;; | 'e               | é                       |
;; | 'i               | í                       |
;; | 'o               | ó                       |
;; | 'u               | ú                       |
;; | "u               | ü                       |
;; | ~n               | ñ                       |
;; | ?                | ¿                       |
;; | !                | ¡                       |
;; | _n               | – (en dash)             |
;; | _m               | — (em dash)             |
;; | _h               | ‐ (hyphen)              |
;; | _H               | ‑ (non-breaking hyphen) |
;; | _-               | − (minus sign)          |
;; | a>               | →                       |
;; | a<               | ←                       |
;; |------------------+-------------------------|

;; Delete Blank Lines
;; http://www.masteringemacs.org/article/removing-blank-lines-buffer
(defun modi/delete-blank-lines ()
  "Call `delete-blank-lines' if no region is selected.
If a region is selected, delete all blank lines in that region."
  (interactive)
  (if (use-region-p)
      (flush-lines "^$" (region-beginning) (region-end))
    (delete-blank-lines)))

(defun modi/space-as-i-mean ()
  "Function to manage white space with `kill-word' operations.

1. If point is at the beginning of the line after possibly some white space,
   remove that white space and re-indent that line.
2. If point is at the end of the line before possibly some white space,
   do not do anything.
3. Otherwise, ensure that there is only 1 white space around the point.

During the whole operation do not change the point position with respect to the
surrounding white space.

abc|   def  ghi <-- point on the left of white space after 'abc'
abc| ghi        <-- point still before white space after calling this function
abc   |def  ghi <-- point on the right of white space before 'def'
abc |ghi        <-- point still after white space after calling this function."
  (interactive)
  (save-excursion ; maintain the initial position of the point w.r.t. space
    (cond ((looking-back "^ *") ; remove extra space at beginning of line
           (just-one-space 0)
           (indent-according-to-mode))
          ((looking-at " *$") ; do nothing when point at end of line
           )
          ((or (looking-at   " ")
               (looking-back " ")) ; adjust space only if it exists
           (just-one-space 1))
          (t ; do nothing otherwise
           ))))
;; Delete extra horizontal white space after `kill-word' and `backward-kill-word'
(advice-add 'kill-word :after (lambda (arg) (modi/space-as-i-mean)))

(defvar modi/whole-buffer-if-not-region-fns '(indent-region
                                              eval-region)
  "List of functions to advice so that they act on the whole buffer if a
region is not selected.")

(defvar modi/whole-buffer-if-not-region-adv-fn-name-format
  "modi/adv-%s--whole-buffer-if-not-region"
  "Format for naming the auto generated advice functions for the functions
listed in `modi/whole-buffer-if-not-region-fns'.")

(defun modi/gen-whole-buffer-if-not-region-adv-fn (symbol)
  "Function to generate a function that applies the function represented by
SYMBOL to the whole buffer if region is not selected."
  (let ((fn-name (intern
                  (format
                   modi/whole-buffer-if-not-region-adv-fn-name-format symbol))))
    `(defun ,fn-name (orig-fn &rest args)
       (save-excursion
         ;; Execute the original SYMBOL function if it is called indirectly.
         ;; Example: We do not want to trigger this advice if `eval-region'
         ;;          is called via `eval-defun'.
         (if (or (not (eq ',symbol this-command))
                 (use-region-p))
             (apply orig-fn args)
           (progn
             (apply orig-fn (list (point-min) (point-max)))
             (message "Executed %s on the whole buffer."
                      (propertize (symbol-name ',symbol)
                                  'face 'font-lock-function-name-face))))))))

(defmacro modi/add-whole-buffer-if-not-region-advice ()
  "Generate advice functions for each fn in `modi/whole-buffer-if-not-region-fns'."
  `(progn ,@(mapcar
             (lambda (x) (modi/gen-whole-buffer-if-not-region-adv-fn x))
             modi/whole-buffer-if-not-region-fns)))
(modi/add-whole-buffer-if-not-region-advice)

;; Advice functions (that originally act on a region) to act on the whole
;; buffer if a region is not selected.
(dolist (fn modi/whole-buffer-if-not-region-fns)
  (let ((adv-fn (intern
                 (format
                  modi/whole-buffer-if-not-region-adv-fn-name-format fn))))
    (advice-add fn :around adv-fn)))

;; Update the `region-extract-function' variable defined in `simple.el'
(setq region-extract-function
      (lambda (delete)
        (when (region-beginning)
          ;; `delete' is set to `'delete-only' in `delete-backward-char' and
          ;;  `delete-forward-char' functions.
          (if (eq delete 'delete-only)
              (delete-region (region-beginning) (region-end))
            ;; `delete' is set to `'delete' in `kill-region' function
            ;; `delete' is set to `nil' in `copy-region-as-kill' and
            ;;  `deactivate-mark' functions.

            ;; When doing `C-u M-w`, `C-u C-w', kill the region
            ;; - with all trailing whitespace removed
            ;; - also replace 2 or more spaces with single spaces
            (if (eq 4 (prefix-numeric-value current-prefix-arg))
                (let ((sel (filter-buffer-substring (region-beginning) (region-end) delete)))
                  (with-temp-buffer
                    (insert sel)
                    ;; Removing trailing whitespace from the whole temp buffer
                    (delete-trailing-whitespace)
                    (goto-char (point-min))
                    (while (re-search-forward "\\s-\\{2,\\}" nil :noerror)
                      (replace-match " "))
                    (buffer-string)))
              (filter-buffer-substring (region-beginning) (region-end) delete))))))

(bind-keys
 :map modi-mode-map
  ;; override the binding of `M-;' for `comment-dwim'
  ("M-;"        . endless/comment-line-or-region)
  ("C-x d"      . delete-region)
  ("C-S-d"      . duplicate-current-line-or-region)
  ("C-x \\"      . align-regexp)  ; align selected region to the entered regexp
  ;; align multiple columns in the selected region. Of course all the selected
  ;; lines must have the same number of columns of groups of non-space characters
  ("C-x |"      . modi/align-columns)
  ("C-k"        . modi/kill-line)
  ;; override the binding of `C-o' for `open-line'
  ("C-o"        . modi/smart-open-line)
  ("C-j"        . modi/pull-up-line)
  ("M-="        . count-words) ; count words in buffer if no region selected
  ("<f9>"       . eval-region)
  ;; override M-backspace to always do `backward-kill-word' using `modi-mode-map'.
  ;; Below is required so that `verilog-mode' does not bind it to `kill-word'.
  ("<M-delete>" . backward-kill-word))
(bind-to-modi-map "=" #'modi/align-to-equals)

;; Comment Commander
;; Usage: Quickly pressing `j' twice will toggle comment on the current line or
;;        region and proceed the cursor to the next line.
;;        Now each consecutive pressing of `j', will toggle the comment on that
;;        line and proceed to the next line. Pressing `p' or `n' will simply
;;        navigate the cursor to the next or previous line without commenting
;;        or uncommenting anything.
;;
;;        jj j j j j j n n n j j p j n
;;
;;        Numeric prefixes are supported too:
;;
;;        jj 5j 7j 2j j 7n n n j j p j n
(defhydra hydra-comment (:color red
                         :columns 4)
  "comment"
  ("j" endless/comment-line-or-region "toggle comment")
  (";" endless/comment-line-or-region "toggle comment")
  ("p" previous-line                  "prev line")
  ("n" next-line                      "next line")
  ("{" backward-paragraph             "backward para")
  ("P" backward-paragraph             "backward para")
  ("}" forward-paragraph              "forward para")
  ("N" forward-paragraph              "forward para")
  ("m" set-mark-command               "set mark")
  ("d" mark-defun                     "mark defun")
  ("k" kill-whole-line                "kill whole line")
  ("b" backward-sexp                  "backward sexp")
  ("f" forward-sexp                   "forward sexp")
  ("q" nil                            "cancel" :color blue))
(key-chord-define-global "jj" #'hydra-comment/body)
(bind-key "C-c ;" #'hydra-comment/body modi-mode-map)


(provide 'setup-editing)


;; TIPS

;; (1) Commented new line
;; `M-j'/`C-M-j' - `indent-new-comment-line' (aliased to `comment-indent-new-line')
;; This creates a commented new line; useful when writing multiline comments
;; like this one without having to manually type in the comment characters.

;; (2) Undo
;; `C-_' or `C-/'

;; (3) Toggle read-only-mode; toggles buffers between editable and read-only states.
;; `C-x C-q' or `M-x read-only-mode'

;; (4) Backups using prefix args to `save-buffer' (C-x C-s)
;;                 C-x C-s -> Makes the prev version into a backup file if
;;                            previously requested or if this is the first save.
;;             C-u C-x C-s -> Marks this version to become a backup when the
;;                            next save is done.
;;         C-u C-u C-x C-s -> Unconditionally makes the previous version into
;;                            a backup file.
;;     C-u C-u C-u C-x C-s -> Does both of what C-u prefix and C-u C-u prefix do.
