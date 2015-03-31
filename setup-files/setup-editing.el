;; Time-stamp: <2015-03-31 16:18:17 kmodi>

;; Functions related to editing text in the buffer

;; Write time stamps when saving files
;; Source: http://www.emacswiki.org/emacs/TimeStamp
;; You can arrange to put a time stamp in a file, so that it is updated
;; automatically each time you edit and save the file. The time stamp must be
;; in the first 8 lines of the file, and you should insert it like this:
;;      Time-stamp: <2013-11-28 02:08:29 KModi>
;; or like this:
;;      Time-stamp: " "
(setq time-stamp-line-limit 20)
(add-hook 'write-file-hooks 'time-stamp)

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

;; Managing white spaces and empty newlines
;; Delete trailing white space in lines and empty new lines at the end of file
;; at the time of saving
;; This is very useful for macro definitions in Verilog as for multi-line
;; macros, NO space is allowed after line continuation character "\"
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; Align
;; http://stackoverflow.com/questions/6217153/aligning-or-prettifying-code-in-emacs
;; http://stackoverflow.com/questions/3633120/emacs-hotkey-to-align-equal-signs
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
  (indent-region begin end)) ;; indent the region correctly after alignment

;; http://stackoverflow.com/a/3035574/1219634
(defun eval-and-replace-last-sexp ()
  "Replace an emacs lisp expression (s-expression aka sexp) with its result.

How to use: Put the cursor at the end of an expression like (+ 1 2) and call
this command."
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%s" value))))
(bind-to-modi-map "x" eval-and-replace-last-sexp)

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

;; Make `kill-whole-line' indentation aware
;; https://github.com/lunaryorn/stante-pede/blob/master/init.el
(defun smart-kill-whole-line (&optional arg)
  "Kill whole line and move back to indentation.
Kill the whole line with function `kill-whole-line' and then move
`back-to-indentation'."
  (interactive "p")
  (kill-whole-line arg)
  (back-to-indentation))

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

;; ;; zap-to-char
;; ;; Source: https://github.com/purcell/emacs.d/blob/master/lisp/init-editing-utils.el
;; (autoload 'zap-up-to-char "misc" "Kill up to, but not including ARGth occurrence of CHAR.")
;; (bind-keys
;;  :map modi-mode-map
;;  ("M-z" . zap-up-to-char)
;;  ("M-Z" . zap-to-char))

;; zop-to-char
;; Source: https://github.com/thierryvolpiatto/zop-to-char
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

;; indent-guide
(use-package indent-guide
  :config
  (progn
    (setq indent-guide-recursive t)
    (setq indent-guide-char "|")

    (defvar modi/indent-guide-mode-hooks '(verilog-mode-hook
                                           emacs-lisp-mode-hook
                                           python-mode-hook
                                           sh-mode-hook
                                           cperl-mode-hook)
      "List of hooks of major modes in which indent-guide-mode should be enabled.")
    (defun turn-on-indent-guide ()
      "Turn on indent-guide-mode only for specific modes."
      (interactive)
      (dolist (hook modi/indent-guide-mode-hooks)
        (add-hook hook #'indent-guide-mode)))
    (defun turn-off-indent-guide ()
      "Turn off indent-guide-mode only for specific modes."
      (interactive)
      (indent-guide-global-mode -1)
      (dolist (hook modi/indent-guide-mode-hooks)
        (remove-hook hook #'indent-guide-mode)))))

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
          (chunk                     (buffer-substring beg end)))
      (setq chunk (concat
                   (format "%s #%-d %s %s %s\n%s "
                           chars-start-of-code-block
                           (line-number-at-pos beg)
                           chars-n-dash
                           (or (buffer-file-name) (buffer-name))
                           chars-m-dash
                           chars-pipe)
                   (replace-regexp-in-string "\n" (format "\n%s " chars-pipe)
                                             chunk)
                   (format "\n%s #%-d %s %s %s %s"
                           chars-end-of-code-block
                           (line-number-at-pos end)
                           chars-n-dash
                           (format-time-string "%Y/%m/%d")
                           chars-n-dash
                           (getenv "USER"))))
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
  :commands (modi/extend-rectangle-to-end)
  :init
  (progn
    (when (featurep 'region-bindings-mode)
      (bind-keys
       :map region-bindings-mode-map
        ("|" . modi/extend-rectangle-to-end))))
  :config
  (progn
    (defun modi/extend-rectangle-to-end(beg end)
      "Make `extend-rectangle-to-end' work for read-only buffers too."
      (interactive "r")
      (let* (original-read-only-state buffer-read-only)
        (when original-read-only-state
          (read-only-mode -1)) ; Force the buffer to be writable
        (extend-rectangle-to-end beg end)
        (when original-read-only-state
          (read-only-mode +1)))))) ; Revert the buffer back to its read-only state

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

(defhydra hydra-change-case(:color red)
  "change-case"
  ("c"   modi/capitalize       "Capitalize")
  ("u"   modi/upcase           "UPCASE")
  ("l"   modi/downcase         "downcase")
  ("M-c" xah-cycle-letter-case "→Cap→UP→down→")
  ("q"   nil                   "cancel" :color blue))

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

;; Forked version of https://github.com/jorgenschaefer/gplusify
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
    (when (not (equal option '(16))) ; C-u C-u
      (insert (concat " - " (getenv "USER"))))
    ;; Insert a space after the time stamp if not at the end of the line
    (when (not (looking-at " *$"))
      (insert " "))))
(bind-key "C-c d" #'modi/insert-time-stamp modi-mode-map)

;; Unicode
(defhydra hydra-unicode (:color blue)
  "unicode"
  (">"       (insert-char ?☛) "☛") ; C-x 8 RET 261b RET, pointing hand
  ("SPC"     (insert-char ?​)  "0-width space") ; C-x 8 RET 200b RET, zero width space
  ("\\"      (insert-char ?▮) "▮") ; C-x 8 RET 9646 RET, black vertical rectangle
  ("|"       (insert-char ?▯) "▯") ; C-x 8 RET 9647 RET, white vertical rectangle
  ("dd"      (insert-char ?“) "“") ; curved double quotes open
  ("df"      (insert-char ?”) "”") ; curved double quotes close
  ("<right>" (insert-char ?→) "→")
  ("<left>"  (insert-char ?←) "←")
  ("<up>"    (insert-char ?↑) "↑")
  ("<down>"  (insert-char ?↓) "↓")
  ("q"   nil              "cancel"))
(bind-key "s-u" #'hydra-unicode/body modi-mode-map)
(key-chord-define-global "jk" #'hydra-unicode/body)

;; Key bindings
(bind-keys
 :map modi-mode-map
  ;; override the binding of `M-;' for `comment-dwim'
  ("M-;"     . endless/comment-line-or-region)
  ("C-x d"   . delete-region)
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
  ("C-j"     . modi/pull-up-line)
  ("M-j"     . comment-indent-new-line)
  ("<f9>"    . eval-region))

;; Bind `what-cursor-position' to `modi-mode-map' as I have overridden its
;; default binding `C-x =' with something else.
(bind-to-modi-map "=" what-cursor-position)

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
(defhydra hydra-comment (:color red)
  "comment"
  ("j"   endless/comment-line-or-region "toggle comment")
  ("p"   previous-line                  "prev line")
  ("n"   next-line                      "next line")
  ("{"   backward-paragraph             "backward para")
  ("P"   backward-paragraph             "backward para")
  ("}"   forward-paragraph              "forward para")
  ("N"   forward-paragraph              "forward para")
  ("m"   set-mark-command               "set mark")
  ("f"   mark-defun                     "mark defun")
  ("k"   smart-kill-whole-line          "kill line")
  ("s b" backward-sexp                  "backward sexp")
  ("s f" forward-sexp                   "forward sexp")
  ("q"   nil                            "cancel" :color blue))
(key-chord-define-global "jj" #'hydra-comment/body)


(provide 'setup-editing)


;; TIPS

;; (1) Commented new line
;; `M-j' - `comment-indent-new-line'
;; This creates a commented new line; useful when writing multiline comments
;; like this one without having to manually type in the comment characters.

;; (2) Undo
;; `C-_' or `C-/'

;; (3) Toggle read-only-mode; toggles buffers between editable and read-only states.
;; `C-x C-q' or `M-x read-only-mode'
