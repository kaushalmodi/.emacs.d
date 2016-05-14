;; Inspired from https://github.com/CodeFalling/nlinum-relative

(defcustom nlinum-plus-current-symbol ""
  "The symbol you want to show on the current line, by default it is empty.

You can use any string like \"->\". If this variable is an empty string,
nlinum-plus will show the real line number at current line."
  :type 'string
  :group 'nlinum-plus)

(defvar nlinum-plus-show-relative nil
  "When non-nil, show relative line numbers.")

(defface nlinum-plus-current-face
  '((t :inherit linum :foreground "#CAE682" :background "#444444" :weight bold))
  "Face for displaying current line."
  :group 'nlinum-plus)

(defvar-local nlinum-plus--current-line 0
  "Store current line number before jit-lock.")

(defun nlinum-plus-reflush ()
  "Reflush display on current window"
  (setq nlinum-plus--current-line (string-to-number (format-mode-line "%l")))
  (remove-overlays (point-min) (point-max) 'nlinum t)
  (with-silent-modifications
    (remove-text-properties (point-min) (point-max) '(fontified))))

(defun nlinum-plus-format-function (line width)
  "Function to replace `nlinum-format-function'."
  (let* ((line-diff (abs (- line nlinum-plus--current-line)))
         (current-line-p (eq line-diff 0))
         (line-display (if current-line-p
                           (if (string= nlinum-plus-current-symbol "")
                               nlinum-plus--current-line
                             nil)
                         (if nlinum-plus-show-relative
                             line-diff
                           line)))
         (str (if line-display
                  (format nlinum-format line-display)
                nlinum-plus-current-symbol)))
    (when (< (length str) width)
      ;; Left pad to try and right-align the line-numbers.
      (setq str (concat (make-string (- width (length str)) ?\ ) str)))
    (if current-line-p
        (put-text-property 0 width 'face 'nlinum-plus-current-face str)
      (put-text-property 0 width 'face 'linum str))
    str))

(defun nlinum-plus-toggle-relative ()
  "Toggle showing relative line numbers."
  (interactive)
  (setq nlinum-plus-show-relative (null nlinum-plus-show-relative)))

;;;###autoload
(define-minor-mode nlinum-plus-mode
  "A minor mode that uses `nlinum-mode' but with few hacks.

- Highlighting the current line number
- Optionally enabling relative numbers."
  :lighter nil
  (if nlinum-plus-mode
      (progn
        (add-function :override nlinum-format-function #'nlinum-plus-format-function)
        (add-hook 'post-command-hook #'nlinum-plus-reflush))
    (progn
      (remove-function nlinum-format-function #'nlinum-plus-format-function)
      (remove-hook 'post-command-hook #'nlinum-plus-reflush))))

;;;###autoload
(define-globalized-minor-mode global-nlinum-plus-mode nlinum-plus-mode
  (lambda () (unless (minibufferp) (nlinum-plus-mode))))


(provide 'nlinum-plus)
