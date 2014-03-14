;; Time-stamp: <2014-03-13 00:00:27 kmodi>

;; iy-go-to-char
;; https://github.com/doitian/iy-go-to-char
(require 'iy-go-to-char)

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


(setq setup-navigation-loaded t)
(provide 'setup-navigation)
