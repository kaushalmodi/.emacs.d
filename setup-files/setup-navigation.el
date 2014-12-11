;; Time-stamp: <2014-12-10 11:25:36 kmodi>

;; iy-go-to-char
;; https://github.com/doitian/iy-go-to-char
(req-package iy-go-to-char
  :require (key-chord multiple-cursors)
  :config
  (progn
    (setq iy-go-to-char-continue-when-repeating t)
    (setq iy-go-to-char-use-key-backward t)
    (setq iy-go-to-char-key-backward ?\,)
    (setq iy-go-to-char-use-key-forward t)
    (setq iy-go-to-char-key-forward ?\.)
    ;; Note that repeatedly calling the `iy-go-to-char' key-chords without first
    ;; quitting the previous `iy-go-to-char' call will cause emacs to crash.
    (key-chord-define-global "]'"  'iy-go-to-char)
    (key-chord-define-global "}\"" 'iy-go-to-or-up-to-continue)
    (key-chord-define-global "[;"  'iy-go-to-char-backward)
    (key-chord-define-global "{:"  'iy-go-to-or-up-to-continue-backward)
    ;; To make `iy-go-to-char' works better with `multiple-cursors', add
    ;; `iy-go-to-char-start-pos' to `mc/cursor-specific-vars' when mc is loaded:
    (eval-after-load 'multiple-cursors
      '(add-to-list 'mc/cursor-specific-vars 'iy-go-to-char-start-pos))))
;; iy-go-to-char
;; Except repeating the char key, followings keys are defined before
;; quitting the search (which can be disabled by setting
;; `iy-go-to-char-override-local-map' to nil):
;;
;;    X   -- where X is the char to be searched. Repeating it will search
;;           forward the char. Can be disabled through
;;           `iy-go-to-char-continue-when-repeating'
;;
;;    ;   -- search forward the char, customizable:
;;           `iy-go-to-char-key-forward', `iy-go-to-char-use-key-forward'
;;
;;    ,   -- search backward the char, customizable:
;;           `iy-go-to-char-key-backward', `iy-go-to-char-use-key-backward'
;;
;;    C-g -- quit
;;
;;    C-s -- start `isearch-forward' using char as initial search
;;           string
;;
;;    C-r -- start `isearch-backward' using char as initial search
;;           string
;;
;;    C-w -- quit and kill region between start and current point.  If region is
;;           activated before search, then use the original mark instead of the
;;           start position.
;;
;;    M-w -- quit and save region between start and current point.  If region is
;;           activated before search, use the mark instead of start position.
;;
;; All other keys will quit the search.  Then the key event is
;; intepreted in the original environment before search.
;;
;; if the search quits because of error or using "C-g", point is set
;; back to the start position.  Otherwise, point is not changed and the
;; start position is set as marker.  So you can use "C-x C-x" back to
;; that position.

;; `iy-go-to-char-backward' search backward by default.  Also the search can
;; cross lines.  To continue search last char, use `iy-go-to-char-continue' and
;; `iy-go-to-char-continue-backward'.


;; Make C-a toggle between beginning of line and indentation
;; https://github.com/lunaryorn/stante-pede/blob/master/init.el
(defun back-to-indentation-or-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; Move more quickly
(defun next-line-fast ()
  "Faster `C-n'"
  (interactive)
  (ignore-errors (next-line 5)))

(defun previous-line-fast ()
  "Faster `C-p'"
  (interactive)
  (ignore-errors (previous-line 5)))

(defun forward-char-fast ()
  "Faster `C-f'"
  (interactive)
  (ignore-errors (forward-char 5)))

(defun backward-char-fast ()
  "Faster `C-b'"
  (interactive)
  (ignore-errors (backward-char 5)))

;; Patched version to fix this issue:
;; In Verilog/C/C++, comments can begin with //.
;; Here's an example comment,
;; //This is a comment
;; I like to use the find-file-at-point feature. If my cursor is on the
;; file name in `include "some_file.v".  But if my cursor is on the above
;; example comment and if I hit C-x C-f, emacs tries to open a tentative
;; path //This!
;; How do I selectively prevent find-file-at-point from activating? In
;; this case, when the major mode is verilog-mode, how do I NOT do
;; find-file-at-point when my cursor is on a line where the first 2
;; non-space characters are //?
;; Source: http://emacs.stackexchange.com/questions/107/how-do-i-disable-ffap-find-file-at-point-when-the-first-two-non-space-characte
(require 'ffap)

;; Patch `ffap-string-at-point-mode-alist' to support file paths with curly braces:
;; ${PRJ_USER}/somefile.txt
;; Delete a list from `ffap-string-at-point-mode-alist' whose `car' is `file'
;; and then add a new list `(file ..)' that supports the curly braces.
(remove-from-alist-matching-car ffap-string-at-point-mode-alist file)
(add-to-list 'ffap-string-at-point-mode-alist '(file "--:\\\\$\\{\\}+<>@-Z_[:alpha:]~*?" "<@" "@>;.,!:"))

(defun ffap-string-at-point (&optional mode)
  (let* ((args
          (cdr
           (or (assq (or mode major-mode) ffap-string-at-point-mode-alist)
               (assq 'file ffap-string-at-point-mode-alist))))
         next-comment ; patch
         modi/ffap-string-at-point-loadeXd
         (pt (point))
         (beg (if (use-region-p)
                  (region-beginning)
                (save-excursion
                  (skip-chars-backward (car args))
                  (skip-chars-forward (nth 1 args) pt)
                  ;; patch
                  (save-excursion
                    (setq next-comment
                          (progn (comment-search-forward (line-end-position) :noerror)
                                 (point))))
                  ;; (message "next-comment = %d" next-comment)
                  ;;
                  (point))))
         (end (if (use-region-p)
                  (region-end)
                (save-excursion
                  (skip-chars-forward (car args))
                  (skip-chars-backward (nth 2 args) pt)
                  (point)))))
    ;; patch
    ;; (message "end = %d beg = %d" end beg)
    (when (> end next-comment)
      (setq beg next-comment))
    (setq modi/ffap-string-at-point-loaded t)
    ;;
    (setq ffap-string-at-point
          (buffer-substring-no-properties
           (setcar ffap-string-at-point-region beg)
           (setcar (cdr ffap-string-at-point-region) end)))))

;; Inspired for this question on emacs.SE: http://emacs.stackexchange.com/q/4271/115
(defun modi/forward-word-begin(arg)
  "Move forward a word and end up with the point being at the beginning of the
next word.  Move point forward ARG words (backward if ARG is negative).
If ARG is omitted or nil, move point forward one word."
  (interactive "p")
  (forward-word arg)
  (forward-word 1)
  (backward-word 1))

;; Key bindings
(bind-keys
 :map modi-mode-map
 ("<f1>" . goto-line)
 ;; override the binding of `C-a` for `move-beginning-of-line'
 ("C-a" . back-to-indentation-or-beginning-of-line)
 ;; Move faster
 ("C-S-n" . next-line-fast)
 ("C-S-p" . previous-line-fast)
 ("C-S-f" . forward-char-fast)
 ("C-S-b" . backward-char-fast)
 ;; Scroll down; does the same as `M-v'. It makes scrolling up and down quick
 ;; as the `scroll-up' is bound to `C-v'.
 ("C-S-v" . scroll-down)
 ;; NOTE: `C-[` key combination is the same as pressing the meta key Esc|Alt
 ;; Do NOT reconfigure that key combination.
 ("C-}" . forward-paragraph)
 ("M-]" . forward-paragraph)
 ("C-{" . backward-paragraph)
 ("M-[" . backward-paragraph)
 ("M-f" . modi/forward-word-begin)
 ("M-F" . forward-word)
 ;; Toggle Follow-mode
 ("C-c f" . follow-mode))
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Follow-Mode.html
;; Follow-mode is a minor mode that makes 2 or more windows, all showing the same
;; buffer/file, scroll as a single tall virtual window. To use Follow mode, go
;; to a frame with just one window, split it into two side-by-side windows using
;; C-x 3, and then type M-x follow-mode. From then on, you can edit the buffer
;; in either of the windows, or scroll either one; the other window follows it.
;; In Follow mode, if you move point outside the portion visible in one window
;; and into the portion visible in the other window, that selects the other
;; window again, treating the two as if they were parts of one large window.

;; The `M-}' and `M-{' bindings are useful in Ibuffer and dired to move to
;; next and previous marked items respectively. So bind them in global map so
;; that those major modes and override the below bindings.
(bind-keys
 ("M-}" . forward-paragraph) ;; default key-binding for `forward-paragraph'
 ("M-{" . backward-paragraph) ;; default key-binding for `backward-paragraph'
 )

(key-chord-define-global "1q" 'goto-line) ;; alternative for F1
(key-chord-define-global "m," 'beginning-of-buffer)
(key-chord-define-global ",." 'end-of-buffer)


(provide 'setup-navigation)
