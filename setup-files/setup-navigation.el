;; Time-stamp: <2016-05-18 19:22:07 kmodi>

(>=e "25.0"
    (setq fast-but-imprecise-scrolling t))

;; Bind `goto-line' in global map to <f1>, but only when running GUI, because
;; `C-h' binding in not available for emacs in no-window mode. And so we will
;; need to retain the default <f1> binding for help functions.
(bind-keys
 :filter (display-graphic-p)
  ("<f1>" . goto-line))

;; iy-go-to-char
;; https://github.com/doitian/iy-go-to-char
(use-package iy-go-to-char
  :defer t
  ;; Note that repeatedly calling the `iy-go-to-char' key-chords without first
  ;; quitting the previous `iy-go-to-char' call will cause emacs to crash.
  :chords (("]'" . iy-go-to-char)
           ("[;" . iy-go-to-char-backward)
           ("}\"" . iy-go-to-or-up-to-continue)
           ("{:" . iy-go-to-or-up-to-continue-backward))
  :config
  (progn
    (setq iy-go-to-char-continue-when-repeating t)
    (setq iy-go-to-char-use-key-backward        t)
    (setq iy-go-to-char-key-backward            ?\,)
    (setq iy-go-to-char-use-key-forward         t)
    (setq iy-go-to-char-key-forward             ?\.)
    ;; To make `iy-go-to-char' works better with `multiple-cursors', add
    ;; `iy-go-to-char-start-pos' to `mc/cursor-specific-vars' when mc is loaded:
    (with-eval-after-load 'multiple-cursors
      (add-to-list 'mc/cursor-specific-vars #'iy-go-to-char-start-pos))))

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


;; https://github.com/lunaryorn/stante-pede/blob/master/init.el
(defun modi/beginning-of-line-or-indentation (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.
If point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")

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

(defun forward-word-fast ()
  "Faster `M-f'"
  (interactive)
  (ignore-errors (forward-word 5)))

(defun backward-word-fast ()
  "Faster `M-b'"
  (interactive)
  (ignore-errors (backward-word 5)))

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
;; http://emacs.stackexchange.com/q/107/115
(use-package ffap
  :commands (find-file-at-point ffap-guess-file-name-at-point)
  :config
  (progn
    (defun ffap-string-at-point (&optional mode)
      (let* ((args
              (cdr
               (or (assq (or mode major-mode) ffap-string-at-point-mode-alist)
                   (assq 'file ffap-string-at-point-mode-alist))))
             next-comment ; patch
             (pt (point))
             (beg (if (use-region-p)
                      (region-beginning)
                    (save-excursion
                      (skip-chars-backward (car args))
                      (skip-chars-forward (nth 1 args) pt)
                      (save-excursion ; patch
                        (setq next-comment ; patch
                              (progn (comment-search-forward (line-end-position) :noerror) ; patch
                                     (point)))) ; patch
                      ;; (message "next-comment = %d" next-comment) ; patch
                      (point))))
             (end (if (use-region-p)
                      (region-end)
                    (save-excursion
                      (skip-chars-forward (car args))
                      (skip-chars-backward (nth 2 args) pt)
                      (point)))))
        ;; (message "end = %d beg = %d" end beg) ; patch
        (when (> end next-comment) ; patch
          (setq beg next-comment)) ; patch
        (setq ffap-string-at-point
              (buffer-substring-no-properties
               (setcar ffap-string-at-point-region beg)
               (setcar (cdr ffap-string-at-point-region) end)))))))

;; Inspired from this emacs.SE question: http://emacs.stackexchange.com/q/4271/115
(defun modi/forward-word-begin(arg)
  "Move forward a word and end up with the point being at the beginning of the
next word.  Move point forward ARG words (backward if ARG is negative).
If ARG is omitted or nil, move point forward one word."
  (interactive "p")
  (forward-word arg)
  (forward-word 1)
  (backward-word 1))

;; https://github.com/abo-abo/hydra/blob/master/hydra-examples.el
;; A three-headed hydra for jumping between "errors", useful for
;; e.g. `occur', `rgrep' and the like.
;; http://www.masteringemacs.org/article/searching-buffers-occur-mode
;; "Another useful feature (of occur) is its support for the compilation mode
;; commands next/previous-error (M-g M-n and M-g M-p respectively), as
;; they enable you to cycle through the list of occur matches from
;; within the source buffer itself."
(defhydra hydra-nav-error
  (nil "M-g"
       :bind (lambda (key cmd) (bind-key key cmd modi-mode-map))
       :color pink)
  "nav-error"
  ("g"        first-error    "first")
  ("n"        next-error     "next")
  ("p"        previous-error "prev")
  ("q"        nil            "cancel")
  ("<return>" nil            "cancel"))

;; Avy Jump
;; https://github.com/abo-abo/avy
(use-package avy
  :commands (modi/goto-line)
  :init
  (progn
    (global-set-key [remap goto-line] #'modi/goto-line))
  :bind (:map isearch-mode-map
         ("M-a" . isearch-avy)) ; isearch > avy
  :bind (:map modi-mode-map
         ("C-c C-SPC" . modi/avy))
  :chords (("l;" . modi/avy))
  :config
  (progn
    (setq avy-style 'pre)
    (setq avy-styles-alist '((avy-goto-line . at-full)))

    (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
    ;; Increase the number of keys for `avy-goto-line' so that for the usual
    ;; case of a 2-window frame, it's possible to jump to any line using just
    ;; 2-char jump code
    (setq avy-keys-alist '((avy-goto-line . (?a ?s ?d ?f ?g ?h ?j ?k ?l ?b ?n ?m))))

    (defalias 'isearch-avy 'avy-isearch) ; for consistency

    (defun modi/avy (arg)
      "Call `avy-goto-word-1' by default.

If ARG is non-nil, call `avy-goto-char-2' instead.
Temporarily disable FCI (if enabled) while the avy functions are executed."
      (interactive "P")
      (let ((avy-all-windows t) ; search in all windows
            (fci-state-orig (when (featurep 'fill-column-indicator) fci-mode))
            (current-prefix-arg nil) ; Don't pass on this wrapper's args to `fn'
            (fn (if arg
                    #'avy-goto-char-2 ; C-u
                  #'avy-goto-word-1)))
        (if fci-state-orig
            (fci-mode 'toggle))
        (call-interactively fn)
        (if fci-state-orig
            (fci-mode 'toggle))))

    (defun modi/goto-line (line)
      "Call `avy-goto-line'.

If LINE is non-nil, go directly to line number LINE.
Temporarily disable FCI (if enabled) while `avy-goto-line' is executed."
      (interactive "P")
      (let ((avy-all-windows t) ; search in all windows
            (fci-state-orig (when (featurep 'fill-column-indicator) fci-mode)))
        (if line
            (goto-line (if (listp line) (car line) line))
          (progn
            (if fci-state-orig
                (fci-mode 'toggle))
            (call-interactively #'avy-goto-line)
            (if fci-state-orig
                (fci-mode 'toggle))))))))

(bind-keys
 ;; Bind in `global-map' to allow the `term-mode-map' to override the `C-a' binding
 ("C-a"   . modi/beginning-of-line-or-indentation) ; default binding for `move-beginning-of-line'
 ;; The `M-}' and `M-{' bindings are useful in Ibuffer and dired to move to
 ;; next and previous marked items respectively. Bind them in global map so
 ;; that those major mode bindings can override the global bindings.
 ("M-}" . forward-paragraph) ; default binding for `forward-paragraph'
 ("M-{" . backward-paragraph)) ; default binding for `backward-paragraph'

(bind-keys
 :map modi-mode-map
  ("M-f"    . forward-word)
  ("M-F"    . forward-word-fast)
  ("M-b"    . backward-word)
  ("M-B"    . backward-word-fast)
  ;; !WARN! `C-[` key combination is the same as pressing the meta key Esc|Alt
  ;; Do NOT reconfigure that key combination.
  ("C-}"    . forward-paragraph)
  ("C-{"    . backward-paragraph))

(bind-keys
 :filter (display-graphic-p)
  ("C-S-a" . move-beginning-of-line))

(bind-keys
 :map modi-mode-map
 :filter (display-graphic-p)
  ;; Move faster
  ("C-S-n" . next-line-fast)
  ("C-S-p" . previous-line-fast)
  ("C-S-f" . forward-char-fast)
  ("C-S-b" . backward-char-fast)
  ;; Scroll down; does the same as `M-v'. It makes scrolling up and down quick
  ;; as the `scroll-up' is bound to `C-v'.
  ("C-S-v" . scroll-down)
  ("M-]" . forward-paragraph)
  ("M-[" . backward-paragraph))

(key-chord-define-global "m," #'beginning-of-buffer)
(key-chord-define-global ",." #'end-of-buffer)


(provide 'setup-navigation)

;; Follow mode
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Follow-Mode.html
;; Follow-mode is a minor mode that makes 2 or more windows, all showing the same
;; buffer/file, scroll as a single tall virtual window. To use Follow mode, go
;; to a frame with just one window, split it into two side-by-side windows using
;; C-x 3, and then type M-x follow-mode. From then on, you can edit the buffer
;; in either of the windows, or scroll either one; the other window follows it.
;; In Follow mode, if you move point outside the portion visible in one window
;; and into the portion visible in the other window, that selects the other
;; window again, treating the two as if they were parts of one large window.

;; Ref: http://debbugs.gnu.org/cgi/bugreport.cgi?bug=19839
;; My below patch `ffap-string-at-point-mode-alist' to support file paths with
;; curly braces like "${HOME}/.emacs.d/init.el" was committed to emacs trunk
;; http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=ba6c32b6decaa2a72a3d5f854efd513e8e82c118
;; on 2015/04/08.
;; (setq ffap-string-at-point-mode-alist
;;       (assq-delete-all 'file ffap-string-at-point-mode-alist))
;; ;; and then add a new list `(file ..)' that supports the curly braces.
;; (add-to-list 'ffap-string-at-point-mode-alist
;;              '(file "--:\\\\$\\{\\}+<>@-Z_[:alpha:]~*?" "<@" "@>;.,!:"))
