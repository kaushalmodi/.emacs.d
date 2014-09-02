;; Time-stamp: <2014-08-13 15:30:54 kmodi>

;; iy-go-to-char
;; https://github.com/doitian/iy-go-to-char
(req-package iy-go-to-char
  :require (key-chord)
  :config
  (progn
    ;; Note that repeatedly calling the `iy-go-tochar' key-chords without first
    ;; quitting the previous `iy-go-to-char' call will cause emacs to crash.
    (key-chord-define-global "]'" 'iy-go-to-char)
    (key-chord-define-global "[;" 'iy-go-to-char-backward)))


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
 ("M-}" . forward-paragraph) ;; default key-binding for `forward-paragraph'
 ("M-]" . forward-paragraph)
 ("C-{" . backward-paragraph)
 ("M-{" . backward-paragraph) ;; default key-binding for `backward-paragraph'
 ("M-[" . backward-paragraph)
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

(key-chord-define-global "1q" 'goto-line) ;; alternative for F1
(key-chord-define-global "m," 'beginning-of-buffer)
(key-chord-define-global ",." 'end-of-buffer)


(provide 'setup-navigation)
